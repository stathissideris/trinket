(ns trinket.inspector
  (:require [trinket]
            [trinket.ui :as ui]
            [trinket.path :as path]
            [trinket.alias :as alias]
            ;;[trinket.perf :as perf]
            [trinket.util :refer [cfuture]]
            [trinket.keys :as keys]
            [trinket.mouse :as mouse]
            [trinket.print :as print]
            [clojure.string :as str])
  (:import [java.awt Toolkit Graphics2D Dimension Cursor]
           [java.awt.event KeyListener KeyEvent MouseListener MouseMotionListener MouseEvent MouseWheelEvent]
           [java.awt.datatransfer StringSelection]
           [javax.swing JPanel JFrame JScrollPane JScrollBar BorderFactory]))

(def debug false)

(set! *warn-on-reflection* true)

(defn dbg [s]
  (when debug (println s)))

(defonce last-inspector (atom nil))

(def default-options {::cursor        []
                      ::expanded      #{[]}
                      ::scale         1
                      ::show-indexes  true
                      ::limit         10
                      ::string-limit  500
                      ::view-position {:x 0 :y 0}})

(defn- lazy? [x] (or (instance? clojure.lang.LazySeq x)
                     (instance? clojure.lang.Cons x)
                     (instance? clojure.lang.LongRange x)))

(defn collection-tag [x]
  (cond
    (map? x)    :map
    (set? x)    :set
    (vector? x) :vector
    (list? x)   :list
    (string? x) :list
    (lazy? x)   :lazy-seq
    (seq? x)    :lazy-seq ;; range for example
    :else       :atom))

(defmulti data->ui (fn [data attr options] (collection-tag data)))

(defn annotation
  ([x]
   (annotation x 8))
  ([x size]
   (ui/text {::ui/text  (str x)
             ::ui/size  size
             ::ui/font  ui/font-regular
             ::ui/color ui/color-index})))

(defn mono-note
  ([x]
   (annotation x 8))
  ([x size]
   (ui/text {::ui/text  (str x)
             ::ui/size  size
             ::ui/font  ui/font-mono
             ::ui/color ui/color-index})))

(defn- indicate-lazy [ui]
  (ui/horizontal {::ui/children [(annotation "L") ui]}))

(defn atom->ui [data
                {::ui/keys [text] :as attr ::keys [vector]}
                {::keys [cursor limit limits hide-collections] :as options}]
  (ui/text
   (merge
    attr
    {::ui/text  (cond text
                      text

                      (and hide-collections
                           (not (string? data))
                           (not (= :atom (collection-tag data))))
                      (print/hidden-collection data vector)

                      :else
                      (print/pr-str-limit data 100 {::print/as-vector vector}))
     ::ui/color (cond (keyword? data) ui/color-keywords
                      (string? data)  ui/color-strings
                      :else           ui/color-text)
     ::tag      (collection-tag data)})))

(defn- aliases-panel [aliases]
  (ui/vertical
   {::ui/children
    (for [[ns alias] (sort-by val aliases)]
      (mono-note (str alias " = " ns) 10))}))

(defn- tabular-data? [data]
  (every? map? data))

(defn- assoc-bounds [component idx last-idx]
  (merge component
         (when (zero? idx) {::first true})
         (when (= idx last-idx) {::last true})))

(defn- table-row [data
                  {::keys [row-idx path offset total-keys]
                   :or    {offset 0}}
                  {::keys [show-indexes]
                   :as    options}]
  (let [last-col-idx (dec (count data))]
    (ui/row
     {::path        (conj path (+ row-idx offset))
      ::table-row   true
      ::ui/children (cons (when show-indexes
                            (-> (annotation (+ row-idx offset))
                                (assoc ::click-path (conj path (+ row-idx offset)))))
                          (map-indexed
                           (fn [col-idx k]
                             (let [path (conj path (+ row-idx offset) col-idx)]
                               (merge
                                (-> (data->ui (get data k)
                                              {::path       path
                                               ::click-path path}
                                              options)
                                    (assoc-bounds col-idx last-col-idx))
                                {::cell true})))
                           total-keys))})))

(defn- total-keys [data]
  (sort (distinct (mapcat keys data))))

(defn- data-table [data {::keys [path lazy length offset] :as attr} options]
  (let [t-keys   (total-keys data)
        nss      (->> t-keys
                      (filter keyword?)
                      (map namespace)
                      (remove nil?)
                      (distinct))
        aliases  (alias/make-aliases nss)
        last-idx (dec (count data))]
    (ui/vertical
     (merge
      attr
      {::display     ::table
       ::path        path
       ::tag         (collection-tag data)
       ::ui/children
       [(-> (annotation (str "TABLE"
                             (if lazy
                               (str " (lazy)")
                               (str " (" length " ROWS, " (count t-keys) " COLUMNS)"))))
            (assoc ::path path ::click-path path))
        (when aliases (aliases-panel aliases))
        (ui/grid
         {::ui/children
          (vec
           (concat
            ;;header
            [(ui/row {::ui/children (into [nil] (map #(atom->ui % {::ui/text (alias/shorten % aliases)
                                                                   ::ui/underline true}
                                                                options)
                                                     t-keys))})]
            ;;rows
            (map-indexed #(-> (table-row %2
                                         {::row-idx    %1
                                          ::path       path
                                          ::offset     offset
                                          ::total-keys t-keys}
                                         options)
                              (assoc-bounds %1 last-idx))
                         data)
            (when (not lazy)
              [(ui/row {::ui/children [nil (ui/text "...")]})])))})]}))))

(defn sequential->ui [data
                      {::keys [path opening closing offset suppress-indexes length indent-str]
                       :or    {offset 0}
                       :as    attr}
                      {::keys [cursor expanded show-indexes tables marked]
                       :as    options}]
  (cond (not (get expanded path))
        (atom->ui data attr options)

        (and (get tables path) (tabular-data? data))
        (data-table data attr options)

        :else
        (let [last-idx (dec (count data))]
          (ui/vertical
           (merge
            attr
            {::tag         (collection-tag data)
             ::path        path
             ::ui/children
             (for [[idx v] (map-indexed vector data)]
               (let [value-path (conj path (+ idx offset))]
                 (ui/horizontal
                  {::ui/children
                   [ ;;opening
                    (if (zero? idx)
                      (-> (ui/text opening) (assoc ::click-path path)) ;; assoc path to allow mouse selection of whole map
                      (ui/text (or indent-str " ")))

                    ;;value
                    (ui/horizontal
                     {::ui/children
                      (remove
                       nil?
                       [(when (and show-indexes (not suppress-indexes))
                          (-> (annotation (str (+ idx offset)))
                              (assoc ::click-path value-path)))
                        (when-let [{::keys    [pred]
                                    ::ui/keys [color]} (get marked path)]
                          (ui/dot {::ui/size 7 ::color color ::ui/visible (pred v)}))
                        (data->ui v (-> {::path       value-path
                                         ::click-path value-path}
                                        (assoc-bounds idx last-idx))
                                  options)])})

                    ;; closing
                    (if (= idx last-idx)
                      (-> (ui/text closing) (assoc ::click-path path ::ui/alignment "sw"))
                      (ui/text " "))]})))})))))

(defn- safe-subs [^String s start end]
  (when s
    (let [len (.length s)]
      (subs s (min start len) (min end len)))))

(defn- data-page [data path {::keys [string-limit limit limits offsets] :as options}]
  (let [offset (get offsets path 0)]
    (if (string? data)
      {:offset offset
       :data   (safe-subs data offset (get limits path string-limit))}
      {:offset offset
       :data   (->> data
                    (drop offset)
                    (take (get limits path limit)))})))

(defmethod data->ui :atom
  [data attr options]
  (atom->ui data attr options))

(defmethod data->ui :lazy-seq
  [data {::keys [path] :as attr} options]
  (let [{:keys [data offset]} (data-page data path options)]
    (indicate-lazy
     (sequential->ui data
                     (assoc attr
                            ::lazy true
                            ::offset offset
                            ::opening "("
                            ::closing ")")
                     options))))

(defmethod data->ui :vector
  [all-data {::keys [path] :as attr} options]
  (let [{:keys [data offset]} (data-page all-data path options)]
    (sequential->ui data
                    (assoc attr
                           ::vector true
                           ::length (count all-data)
                           ::offset offset
                           ::opening "["
                           ::closing "]")
                    options)))

(defmethod data->ui :list
  [all-data {::keys [path] :as attr} options]
  (let [{:keys [data offset]} (data-page all-data path options)]
    (sequential->ui data
                    (assoc attr
                           ::length (count all-data)
                           ::offset offset
                           ::opening "("
                           ::closing ")")
                    options)))

(defmethod data->ui :set
  [data attr options]
  (sequential->ui data
                  (assoc attr
                         ::opening "#{"
                         ::closing "}"
                         ::indent-str "  "
                         ::suppress-indexes true)
                  options))

(defn- prefixed-map?
  "Checks whether the map's keys are uniformly namespaced keywords"
  [m]
  (and (not (empty? m))
       (every? keyword? (keys m))
       (not (nil? (namespace (ffirst m))))
       (apply = (map namespace (keys m)))))

(defmethod data->ui :map
  [data {::keys [path] :as attr} {::keys [expanded] :as options}]
  (if-not (get expanded path)
    (atom->ui data attr options)
    (let [last-idx (dec (count data))
          prefixed  (prefixed-map? data)]
      (ui/vertical
       (merge
        attr
        {::tag         (collection-tag data)
         ::path        path
         ::ui/children
         [(when prefixed (ui/text {::ui/text    (str "#:" (namespace (ffirst data)))
                                   ::click-path path}))
          (ui/grid
           {::ui/class    "main-map-grid"
            ::ui/children
            (for [[idx [k v]] (map-indexed vector data)]
              (let [key-path (conj path idx ::path/key)
                    val-path (conj path idx ::path/val)]
                (ui/row
                 {::ui/children
                  [ ;;opening
                   (if (zero? idx)
                     (-> (ui/text "{")
                         (merge {::click-path path ::ui/class "opening-brace"})) ;;assoc path to allow mouse selection of whole map
                     (ui/text " "))

                   ;;key
                   (if (get expanded key-path)
                     (-> (data->ui k {::path key-path} options)
                         (assoc-bounds idx last-idx)
                         (merge {::ui/class "map-key"}))
                     (let [k-text (if prefixed
                                    (str ":" (name k))
                                    (pr-str k))]
                       (-> (data->ui k {::path key-path} options)
                           (assoc-bounds idx last-idx)
                           (merge {::ui/text k-text ::ui/class "map-key" ::click-path key-path}))))

                   (ui/text " ")

                   ;;value
                   (-> (data->ui v {::path val-path} options)
                       (assoc-bounds idx last-idx)
                       (merge {::ui/class "map-value" ::click-path val-path}))

                   ;; closing
                   (if (= idx last-idx)
                     (-> (ui/text "}")
                         (merge {::click-path path ::ui/alignment "sw" ::ui/class "closing-brace"}))
                     (ui/text " "))]})))})]})))))

(defn paint-cursor! [ui path ^Graphics2D g]
  (when-let [match (ui/find-component ui #(= path (::path %)))]
    (let [cursor (ui/grow-bounds match 1)]
      (doto g
        (.setColor ui/color-selection-background)
        (.fillRect (int (::ui/ax cursor))
                   (int (::ui/ay cursor))
                   (int (::ui/w cursor))
                   (int (::ui/h cursor)))))))

(defn ->clipboard [s]
  (-> (Toolkit/getDefaultToolkit)
      .getSystemClipboard
      (.setContents (StringSelection. s) nil))
  s)

(defn- set-options!
  ([options]
   (set-options! nil options))
  ([inspector options]
   (let [{:keys [options-atom]} (or inspector @last-inspector)]
     (reset! options-atom options))
   nil)) ;;prevent print explosion

(defn mark!
  ([pred]
   (mark! nil pred))
  ([inspector pred]
   (let [{:keys [options-atom]} (or inspector @last-inspector)]
     (swap! options-atom assoc-in [::marked (::cursor @options-atom) ::pred] pred))
   nil))

(defn unmark!
  ([]
   (unmark! nil))
  ([inspector]
   (let [{:keys [options-atom]} (or inspector @last-inspector)]
     (swap! options-atom update ::marked dissoc (::cursor @options-atom)))
   nil))

(defn unmark-all!
  ([]
   (unmark-all! nil))
  ([inspector]
   (let [{:keys [options-atom]} (or inspector @last-inspector)]
     (swap! options-atom dissoc ::marked))
   nil))

(defn- swap-options! [{:keys [options-atom] :as inspector} & args]
  (apply swap! options-atom args)
  nil) ;;prevent print explosion

(defn- expanded? [{:keys [options-atom] :as inspector} path]
  (get (::expanded @options-atom) path))

(defn- expand! [inspector path]
  (swap-options! inspector update ::expanded (fnil conj #{}) path))

(defn- collapse! [inspector path]
  (swap-options! inspector update ::expanded (fnil disj #{}) path))

(defn- toggle-expansion! [inspector path]
  (when path
    (if (expanded? inspector path)
      (collapse! inspector path)
      (expand! inspector path))))

(defn- safe-dec [x]
  (max 0 ((fnil dec 0) x)))

(def safe-inc (fnil inc 0))
(def safe+ (fnil + 0))

(defn- scroll-seq! [inspector path fun]
  (swap-options! inspector update-in [::offsets path] fun))

(defn- cursor [{:keys [options-atom] :as inspector}]
  (::cursor @options-atom))

(defn- parent-component [ui path]
  (when-let [parent-path (path/up path)]
    (ui/find-component ui #(= parent-path (::path %)))))

(defn- allowed-step* [data step offset limit]
  (if (> limit (->> data (drop (+ offset step)) (take limit) count)) ;;TODO realization here, what about slow ones?
    0
    step))

(defn- allowed-step [data step {::keys [cursor offsets limits limit string-limit]}]
  (let [data   (path/get-in data cursor)
        limit  (or (get limits cursor)
                   (if (string? data) string-limit limit))
        offset (or (get offsets cursor) 0)]
    (allowed-step* data step offset limit)))

(defn- move-cursor! [{:keys [data-atom ui-atom options-atom] :as inspector} direction]
  (let [ui       @ui-atom
        options  @options-atom
        cur      (cursor inspector)
        cur-comp (ui/find-component ui #(= cur (::path %)))
        offset   (-> options ::offsets (get cur 0))]
    (cond
      ;;TABLE navigation
      (and (= :right direction) (::cell cur-comp) (not (::last cur-comp)))
      (swap-options! inspector update ::cursor path/right)

      (and (= :left direction) (::cell cur-comp))
      (if (zero? (last cur))
        (swap-options! inspector update ::cursor path/up) ;;left to get out of row
        (swap-options! inspector update ::cursor path/left))

      (and (= :down direction) (::cell cur-comp))
      (do
        (when (::last (parent-component ui cur)) ;; when the cell is in the last row
          (scroll-seq! inspector (-> cur path/up path/up) safe-inc)) ;; ...scroll down
        (swap-options! inspector update ::cursor path/next-row))

      (and (= :up direction) (::cell cur-comp))
      (do
        (when (::first (parent-component ui cur)) ;; when the cell is in the first row
          (scroll-seq! inspector (-> cur path/up path/up) safe-dec)) ;; ...scroll up
        (swap-options! inspector update ::cursor path/previous-row))

      ;; going in!
      (= :in direction)
      (let [tag (::tag cur-comp)]
        (when (and (not= tag :atom) (or (expanded? inspector cur) (::table-row cur-comp)))
          (if (= tag :map)
            (swap-options! inspector update ::cursor conj 0 ::path/key)
            (swap-options! inspector update ::cursor conj offset))))

      ;; left to go from map value to map key
      (and (= :left direction) (path/val? cur))
      (do
        (dbg "left to go from map value to map key")
        (swap-options! inspector update ::cursor path/point-to-key))

      ;; right to go from map key to map value
      (and (= :right direction) (path/key? cur))
      (do
        (dbg "right to go from map key to map value")
        (swap-options! inspector update ::cursor path/point-to-val))

      ;; up to go to previous key or value
      (= :up direction)
      (do ;; make purer
        (when (::first cur-comp) (scroll-seq! inspector (path/up cur) safe-dec))
        (swap-options! inspector update ::cursor path/left))

      ;; down to go to next key or value
      (= :down direction)
      (do
       (when (::last cur-comp)
         (if (< 0 (allowed-step @data-atom 1 (update options ::cursor path/up)))
           (scroll-seq! inspector (path/up cur) safe-inc)))
       (when (not (::last cur-comp))
         (swap-options! inspector update ::cursor path/right)))

      ;; left to get out of structure
      (and (#{:left :up} direction) (::first cur-comp))
      (do
        (dbg "left to get out of structure")
        (swap-options! inspector update ::cursor path/up))

      ;; left to go to top of structure
      (and (= :left direction) (not (::first cur-comp)))
      (do
        (dbg "left to go to top of structure")
        (swap-options! inspector update ::cursor
                       path/jump (-> options ::offsets (get (path/up cur) 0))))

      ;; right to go into structure
      (= :right direction)
      (let [tag (::tag cur-comp)]
        (dbg "right or down to go into structure")
        (when (and (not= tag :atom) (expanded? inspector cur))
          (if (= tag :map)
            (swap-options! inspector update ::cursor conj 0 ::path/key)
            (swap-options! inspector update ::cursor conj offset))))

      :else
      nil)))

(defn- mouse-clicked [{:keys [ui-atom options-atom] :as inspector} ^MouseEvent e]
  (when-let [match (ui/component-at-point
                    {::ui/x (.getX e) ::ui/y (.getY e)}
                    (ui/scale @ui-atom (::scale @options-atom)))] ;;scale so that clicks land correctly ;;TODO OPTIMIZE!!!
    (condp = (.getClickCount e)
      1 (when-let [cp (::click-path match)]
          (swap-options! inspector assoc ::cursor cp))
      2 (when-not (= :atom (::tag match))
          (toggle-expansion! inspector (::click-path match)))
      nil)))

(defn- mouse-moved [inspector keyboard-atom e]
  (when (keys/pressed? @keyboard-atom :space)
    (prn "Scrolling!")))

(defn mouse-position-printer []
  (proxy [MouseMotionListener] []
    (mouseDragged [e])
    (mouseMoved [^MouseEvent e]
      (println "X:" (.getX e) "Y:" (.getY e)))))

(defn- show-less! [inspector path {::keys [limit]}]
  (swap-options! inspector update-in [::limits path]
                 (fn [x]
                   (max 1 (dec (or x limit))))))

(defn- show-more! [inspector path {::keys [limit]}]
  (swap-options! inspector update-in [::limits path]
                 (fn [x]
                   (inc (or x limit)))))

(defn- focused [{:keys [ui-atom] :as inspector}]
  (let [ui  @ui-atom
        cur (cursor inspector)]
    (ui/find-component ui #(= cur (::path %)))))

(defn- value-at-cursor [data {::keys [cursor offsets] :as options}]
  (path/get-in data cursor))

(defn- def-value-at-cursor! [{:keys [data-atom options-atom] :as inspector}]
  (let [val (value-at-cursor @data-atom @options-atom)]
    (alter-var-root #'trinket/x (fn [_] val))))

(defn- copy-value-at-cursor! [{:keys [data-atom options-atom] :as inspector}]
  (let [val (value-at-cursor @data-atom @options-atom)]
    (->clipboard (pr-str val))))

(defn- key-pressed [{:keys [data-atom ui-atom options-atom ^JFrame frame] :as inspector} e]
  (let [cur       (cursor inspector)
        expand-fn (fn []
                    (when-not (= :atom (::tag (ui/find-component @ui-atom #(= cur (::path %)))))
                      (toggle-expansion! inspector (cursor inspector))))]
    (condp = (:code e)
      :space  (.setCursor frame (Cursor/HAND_CURSOR))
      :tab    (expand-fn)
      :enter  (move-cursor! inspector :in)

      :h      (swap-options! inspector update ::hide-collections not)

      :t      (swap-options! inspector update ::tables
                             (fn [tables]
                               (let [path   (cursor inspector)
                                     tables (or tables #{})]
                                 (if (get tables path)
                                   (disj tables path)
                                   (conj tables path)))))

      :comma  (if (:shift e)
                (scroll-seq! inspector (cursor inspector) #(max 0 (- % 10)))
                (scroll-seq! inspector (cursor inspector) safe-dec))
      :period (let [step (allowed-step @data-atom (if (:shift e) 10 1) @options-atom)]
                (scroll-seq! inspector (cursor inspector) #(safe+ % step)))

      :left   (move-cursor! inspector :left)
      :right  (move-cursor! inspector :right)
      :up     (move-cursor! inspector :up)
      :down   (move-cursor! inspector :down)

      :u      (unmark! inspector)
      :i      (swap-options! inspector update ::show-indexes not)
      :f      (if (:shift e)
                (swap-options! inspector assoc ::focus [])
                (swap-options! inspector #(-> %
                                              (assoc ::focus cur)
                                              (assoc ::cursor []))))

      :0      (when (:shortcut e)
                (swap-options! inspector update ::scale (constantly 1)))
      :equals (if (:shortcut e)
                (swap-options! inspector update ::scale #(+ % 0.1))
                (show-more! inspector (cursor inspector) @options-atom))
      :minus  (if (:shortcut e)
                (swap-options! inspector update ::scale #(let [s (- % 0.1)] (if (< s 0.6) 0.6 s)))
                (show-less! inspector (cursor inspector) @options-atom))

      :d      (def-value-at-cursor! inspector)
      :c      (copy-value-at-cursor! inspector)

      nil)))

(defn key-released [{:keys [^JFrame frame] :as inspector} e]
  (condp = (:code e)
    :space  (.setCursor frame (Cursor/DEFAULT_CURSOR))
    nil))

(defrecord Inspector [data-atom options-atom ui-atom frame])

(defn- make-new-ui [data {::keys [focus]
                          :or {focus []}
                          :as options}]
  (-> data
      (path/get-in focus)
      (data->ui {::path []} options)
      (assoc ::ui/x 10 ::ui/y 10)
      ui/layout
      ui/add-absolute-coords))

(defn- only-diff? [old new k]
  (and (not= (get old k) (get new k))
       (= (dissoc old k)
          (dissoc new k))))

(defn- atom? [x]
  (instance? clojure.lang.Atom x)) ;;TODO make this more generic

(defn- paint-scrollbars! [^JScrollPane sp [pw ph] ^Graphics2D g]
  (let [vp  (.getViewport sp)
        vw  (.getWidth vp)
        vh  (.getHeight vp)
        hor (-> sp .getHorizontalScrollBar .getValue)
        ver (-> sp .getVerticalScrollBar .getValue)]
    ;;horizonal scrollbar
    (when (> pw vw)
      (.fillRect g (* 2 hor) (+ vh ver -10) 10 10))

    ;;vertical scrollbar
    (when (> ph vh)
      (let [w 10
            h (* vh (/ vh ph))
            x (- vw w)
            y (+ ver (* vh (/ ver ph)))]
        (.fillRect g x y w h)))))

(defn- scroll-to! [{:keys [^JFrame frame] :as inspector} [x y]]
  (let [^javax.swing.JViewport vp (-> frame .getContentPane .getComponents first .getViewport)]
    (.setViewPosition vp (ui/point (max 0 x) (max 0 y)))
    (.repaint frame)))

(defn- scroll-by! [{:keys [^JFrame frame] :as inspector} [dx dy]]
  (let [^javax.swing.JViewport vp (-> frame .getContentPane .getComponents first .getViewport)
        ^java.awt.Point p (.getViewPosition vp)
        cx (.getX p)
        cy (.getY p)]
    (.setViewPosition vp (ui/point (max 0 (+ cx dx)) (max 0 (+ cy dy))))
    (.repaint frame)))

(defn inspector
  ([data]
   (inspector data {}))
  ([data options]
   (let [data-atom       (if (atom? data) data (atom data))
         options-atom    (atom (merge default-options options))
         ui-atom         (atom (make-new-ui @data-atom @options-atom))
         sp              (atom nil)
         ^JPanel panel   (proxy [JPanel] []
                           (paintComponent [^Graphics2D g]
                             (try
                               (let [{::ui/keys [w h] :as ui} @ui-atom
                                     scale                    (::scale @options-atom)
                                     f                        scale
                                     ^JScrollPane sp          @sp
                                     scroll-pos               (-> sp .getViewport .getViewPosition)
                                     sx                       (.-x scroll-pos)
                                     sy                       (.-y scroll-pos)
                                     view-size                (-> sp .getViewport .getViewSize)
                                     vw                       (.-width view-size)
                                     vh                       (.-height view-size)]
                                 (.setPreferredSize ^JPanel this (Dimension. (+ 10 (* scale w)) (+ 10 (* scale h))))
                                 (doto g
                                   (.setClip (- sx 2) (- sy 2) (+ 10 vw) (+ 10 vh))
                                   (.scale f f)
                                   (.setColor ui/color-background)
                                   (.fillRect -2 -2
                                              (* (/ 1.0 scale) (+ 10 (.getWidth ^JPanel this)))
                                              (* (/ 1.0 scale) (+ 10 (.getHeight ^JPanel this)))))
                                 (#'paint-cursor! ui (::cursor @options-atom) g)
                                 (ui/paint! ui g)
                                 (.scale g 1 1)
                                 (paint-scrollbars! sp [w h] g))
                               (catch Exception e
                                 (.printStackTrace e)))))
         ^JScrollPane sp (reset! sp (doto (JScrollPane. panel)
                                      (.setVerticalScrollBarPolicy JScrollPane/VERTICAL_SCROLLBAR_NEVER)
                                      (.setHorizontalScrollBarPolicy JScrollPane/HORIZONTAL_SCROLLBAR_NEVER)
                                      ((fn [sp]
                                         (.setUnitIncrement (.getVerticalScrollBar ^JScrollPane sp) 16)
                                         (.setUnitIncrement (.getHorizontalScrollBar ^JScrollPane sp) 8)))))
         ^JFrame frame   (doto (JFrame. "trinket")
                           (.add sp)
                           (.setSize 400 600))
         inspector       (->Inspector data-atom options-atom ui-atom frame)]

     ;;connected atoms
     (add-watch data-atom ::inspector-ui
                (fn [_ _ _ data]
                  (let [{::keys [scale] :as options} @options-atom]
                    (cfuture
                     (let [new-ui (make-new-ui data options)]
                       (reset! ui-atom new-ui)
                       (ui/later (.repaint frame)))))))

     (add-watch options-atom ::inspector-ui
                (fn [_ _ old-options {::keys [scale] :as options}]
                  (cfuture
                   (if (only-diff? old-options options ::cursor)
                     (ui/later (.repaint frame))
                     (let [new-ui (make-new-ui @data-atom options)]
                       (reset! ui-atom new-ui)
                       (ui/later (.repaint frame)))))))

     ;;listeners
     (let [{:keys [key-listener key-atom]}
           (keys/listener {:pressed  (partial key-pressed inspector)
                           :released (partial key-released inspector)})]
       (doto frame
         (.setFocusTraversalKeysEnabled false) ;; so that <TAB> can be detected
         (.addKeyListener key-listener)
         (.setVisible true))
       (let [ml (mouse/listener
                 {:clicked     (partial mouse-clicked inspector)
                  :moved       (partial mouse-moved inspector key-atom)
                  :wheel-moved #(scroll-by! inspector [0 (* 15 (.getWheelRotation ^MouseWheelEvent %))])})]
         (doto panel
           (.setBorder (BorderFactory/createEmptyBorder))
           (.addMouseListener ml)
           (.addMouseMotionListener ml)
           (.addMouseWheelListener ml)
           ;;(.addMouseMotionListener (mouse-position-printer))
           )))

     (reset! last-inspector inspector)

     ;; to initialize preferred size
     ;;(trigger-repaint @ui-atom (::scale @options-atom) panel frame)

     inspector)))

(defn inspect
  "Inspect the passed data."
  ([data]
   (inspect data nil))
  ([data options]
   (inspector data options)
   nil)) ;;prevent print explosion

(defn- ^JFrame last-frame []
  (:frame @last-inspector))

(defn set-data!
  "Sets the data of an inspector. With single arity, it sets the data of
  the last opened inspector. Caution: If you are inspecting an atom,
  this function will mutate your atom."
  ([data]
   (set-data! nil data))
  ([inspector data]
   (let [{:keys [data-atom]} (or inspector @last-inspector)]
     (cond (not data-atom) ;; inspect if not already inspecting
           (do
             (dbg "Wasn't inspecting, opening new window")
             (inspect data))

           (and (not inspector)
                (not (.isVisible (last-frame))))
           (do (dbg "Reusing closed inspector window")
               (reset! data-atom data)
               (.setVisible (last-frame) true))

           :else
           (reset! data-atom data)))
   nil)) ;;prevent print explosion
