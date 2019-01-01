(ns trinket.inspector
  (:require [trinket.ui :as ui]
            [trinket.path :as path]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.zip :as zip])
  (:import [java.awt Toolkit Graphics2D Dimension]
           [java.awt.event KeyListener KeyEvent MouseListener MouseEvent]
           [java.awt.datatransfer StringSelection]
           [javax.swing JPanel JFrame JScrollPane JScrollBar BorderFactory]))

(set! *warn-on-reflection* true)

(def last-inspector (atom nil))

(def default-options {::cursor       []
                      ::expanded     #{[]}
                      ::scale        1
                      ::show-indexes true
                      ::page-length  10})

(defn- lazy? [x] (or (instance? clojure.lang.LazySeq x)
                     (instance? clojure.lang.Cons x)))

(defn collection-tag [x]
  (cond
    (map? x)                        :map
    (set? x)                        :set
    (vector? x)                     :vector
    (list? x)                       :list
    (string? x)                     :list
    (lazy? x)                       :lazy-seq
    :else                           :atom))

(defmulti data->ui (fn [data path options] (collection-tag data)))

(defn annotation [x]
  (ui/text {::ui/text  (str x)
            ::ui/size  8
            ::ui/font  ui/font-regular
            ::ui/color ui/color-index}))

(defn- indicate-lazy [ui]
  (ui/map->Horizontal {::ui/children [(annotation "L") ui]}))

(defn- config-component [ui data path {::keys [idx last-idx cursor]}]
  (cond-> ui
    :always (assoc ::path path
                   ::index idx
                   ::tag (collection-tag data))
    (and idx (zero? idx)) (assoc ::first true)
    (and idx (= idx last-idx)) (assoc ::last true)
    (and cursor (= cursor path)) (assoc ::cursor true)))

(defn atom->ui [data path {::keys [text page-length] :as options}]
  (let [color (cond (keyword? data) ui/color-keywords
                    (string? data)  ui/color-strings
                    :else           ui/color-text)]
    (-> (ui/text {::ui/text  (if text text
                                 (binding [*print-length* page-length]
                                   (pr-str data)))
                  ::ui/color color})
        (config-component data path options))))

(defn- tabular-data? [data]
  (every? map? data))

(defn- data-table [data path {::keys [offset]
                              :as    options
                              :or    {offset 0}}]
  (let [total-keys (sort (distinct (mapcat keys data)))]
    (-> (ui/map->Vertical
         {::ui/children
          [(-> (annotation "TABLE")
               (assoc ::path path))
           (ui/map->Grid
            {::ui/columns (inc (count total-keys))
             ::ui/children
             (concat [nil] (map #(atom->ui % nil options) total-keys)
                     (mapcat (fn [idx row]
                               (cons (annotation (+ offset idx)) (map #(atom->ui (get row %) nil options) total-keys)))
                             (range) data))})]})
        (config-component data path options))))

(defn sequential->ui [data path {::keys [cursor expanded opening closing indent-str show-indexes idx last-idx offset
                                         suppress-indexes tables]
                                 :or {offset 0}
                                 :as options}]
  (cond (not (get expanded path))
        (atom->ui data path (merge options {::idx idx ::last-idx last-idx ::cursor cursor}))

        (and (get tables path) (tabular-data? data))
        (data-table data path options)

        :else
        (let [last-idx (dec (count data))]
          (ui/map->Vertical
           {::cursor      (= cursor path)
            ::tag         (collection-tag data)
            ::ui/children
            (for [[idx v] (map-indexed vector data)]
              (let [value-path (conj path idx)]
                (ui/map->Horizontal
                 {::ui/children
                  [;;opening
                   (if (zero? idx)
                     (-> (ui/text opening) (assoc ::path path)) ;; assoc path to allow mouse selection of whole map
                     (ui/text (or indent-str " ")))

                   ;;value
                   (let [value-ui (let [options (dissoc options ::suppress-indexes)] ;;suppress-indexes is for one-level only
                                    (if (get expanded value-path)
                                      (data->ui v value-path (dissoc options ::indent-str ::offset)) ;; no need to inherit this
                                      (data->ui v value-path (assoc options ::idx idx ::last-idx last-idx ::cursor cursor))))]
                     (if (and show-indexes (not suppress-indexes))
                       (ui/map->Horizontal
                        {::ui/children
                         [(annotation (str (+ idx offset)))
                          value-ui]})
                       value-ui))

                   ;; closing
                   (if (= idx last-idx)
                     (-> (ui/text closing) (assoc ::path path))
                     (ui/text " "))]})))}))))

(defn- data-page [data path {::keys [page-length lengths offsets] :as options}]
  (let [offset (get offsets path 0)]
    {:offset offset
     :data   (->> data
                  (drop offset)
                  (take (get lengths path page-length)))}))

(defn lazy->ui [data path {::keys [expanded page-length lengths offsets] :as options}]
  (let [{:keys [data offset]} (data-page data path options)]
    (indicate-lazy (sequential->ui data path (assoc options ::offset offset)))))

(defmethod data->ui :atom
  [data path options]
  (atom->ui data path options))

(defmethod data->ui :lazy-seq
  [data path options]
  (lazy->ui data path (assoc options ::opening "(" ::closing ")")))

(defmethod data->ui :vector
  [data path options]
  (sequential->ui data path (assoc options ::opening "[" ::closing "]")))

(defmethod data->ui :list
  [data path options]
  (sequential->ui data path (assoc options ::opening "(" ::closing ")")))

(defmethod data->ui :set
  [data path options]
  (sequential->ui data path (assoc options
                                   ::opening "#{" ::closing "}" ::indent-str "  "
                                   ::suppress-indexes true)))

(defmethod data->ui :map
  [data path {::keys [cursor expanded idx last-idx] :as options}]
  (if-not (get expanded path)
    (atom->ui data path options)
    (let [last-idx (dec (count data))]
      (ui/map->Grid
       {::cursor  (= cursor path)
        ::tag     (collection-tag data)
        ::ui/columns 5
        ::ui/children
        (apply concat
         (for [[idx [k v]] (map-indexed vector data)]
           (let [key-path (conj path idx ::path/key)
                 val-path (conj path idx ::path/val)]
             [ ;;opening
              (if (zero? idx)
                (-> (ui/text "{") (assoc ::path path)) ;;assoc path to allow mouse selection of whole map
                (ui/text " "))

              ;;key
              (if (get expanded key-path)
                (data->ui k key-path (assoc options ::idx idx ::last-idx last-idx))
                (data->ui k key-path (assoc options ::text (pr-str k) ::idx idx ::last-idx last-idx ::cursor cursor)))

              (ui/text " ")

              ;;value
              (if (get expanded val-path)
                (data->ui v val-path (assoc options ::idx idx ::last-idx last-idx))
                (data->ui v val-path (assoc options ::idx idx ::last-idx last-idx ::cursor cursor)))

              ;; closing
              (if (= idx last-idx)
                (-> (ui/text "}") (assoc ::path path))
                (ui/text " "))])))}))))

(defn paint-cursor [ui ^Graphics2D g]
  (when-let [match (ui/find-component ui ::cursor)]
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

(defn- set-data!
  ([data]
   (set-data! nil data))
  ([inspector data]
   (let [{:keys [data-atom]} (or inspector @last-inspector)]
     (reset! data-atom data))
   nil)) ;;prevent print explosion

(defn- set-options!
  ([options]
   (set-options! nil options))
  ([inspector options]
   (let [{:keys [options-atom]} (or inspector @last-inspector)]
     (reset! options-atom options))
   nil)) ;;prevent print explosion

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

(defn- cursor [{:keys [options-atom] :as inspector}]
  (::cursor @options-atom))

(defn- move-cursor! [{:keys [ui-atom] :as inspector} direction]
  (let [ui @ui-atom]
    (cond
      ;; going in!
      (= :in direction)
      (let [tag (::tag (ui/find-component ui ::cursor))]
        (when (and (not= tag :atom) (expanded? inspector (cursor inspector)))
          (if (= tag :map)
            (swap-options! inspector update ::cursor conj 0 ::path/key)
            (swap-options! inspector update ::cursor conj 0))))

      ;; left to go from map value to map key
      (and (= :left direction) (path/val? (cursor inspector)))
      (swap-options! inspector update ::cursor path/point-to-key)

      ;; right to go from map key to map value
      (and (= :right direction) (path/key? (cursor inspector)))
      (swap-options! inspector update ::cursor path/point-to-val)

      ;; up to go to previous key or value
      (and (= :up direction) (not (::first (ui/find-component ui ::cursor))))
      (swap-options! inspector update ::cursor path/left)

      ;; down to go to next key or value
      (and (= :down direction) (not (::last (ui/find-component ui ::cursor))))
      (swap-options! inspector update ::cursor path/right)

      ;; down on the last element to go up again - disabled for now, I think this is confusing
      ;; (and (#{:down :right} direction) (::last (ui/find-component ui ::cursor)))
      ;; (swap-options! inspector update ::cursor path/up)

      ;; left to get out of structure
      (and (#{:left :up} direction) (::first (ui/find-component ui ::cursor)))
      (swap-options! inspector update ::cursor path/up)

      ;; left to go to top of structure
      (and (= :left direction) (not (::first (ui/find-component ui ::cursor))))
      (swap-options! inspector update ::cursor path/first)

      ;; right to go to bottom of structure
      ;;(and (= :right direction) (not (::last (ui/find-component ui ::cursor))))
      ;;(swap-options! inspector update ::cursor path/last)

      ;; right or down to go into structure
      (#{:right :down} direction)
      (let [tag (::tag (ui/find-component ui ::cursor))]
        (when (and (not= tag :atom) (expanded? inspector (cursor inspector)))
          (if (= tag :map)
            (swap-options! inspector update ::cursor conj 0 ::path/key)
            (swap-options! inspector update ::cursor conj 0))))

      :else nil)))

(defn- mouse-clicked [{:keys [ui-atom options-atom] :as inspector} ^MouseEvent e]
  (when-let [match (ui/component-at-point
                    {::ui/x (.getX e) ::ui/y (.getY e)}
                    (ui/scale @ui-atom (::scale @options-atom)))] ;;scale so that clicks land correctly ;;TODO OPTIMIZE!!!
    (condp = (.getClickCount e)
      1 (swap-options! inspector assoc ::cursor (::path match))
      2 (when-not (= :atom (::tag match))
          (toggle-expansion! inspector (::path match)))
      nil)))

(defn mouse-listener [{:keys [ui-atom] :as inspector}]
  (proxy [MouseListener] []
    (mouseClicked [e] (#'mouse-clicked inspector e))
    (mouseEntered [e])
    (mouseExited [e])
    (mousePressed [e])
    (mouseReleased [e])))

(defn- is-shortcut-down? [^KeyEvent e]
  (.isMetaDown e))


(defn- safe-dec [x]
  (max 0 ((fnil dec 0) x)))

(def safe-inc (fnil inc 0))

(defn- scroll-seq! [inspector path fun]
  (swap-options! inspector update-in [::offsets path] fun))

(defn- show-less! [inspector path {::keys [page-length]}]
  (swap-options! inspector update-in [::lengths path]
                 (fn [x]
                   (max 1 (dec (or x page-length))))))

(defn- show-more! [inspector path {::keys [page-length]}]
  (swap-options! inspector update-in [::lengths path]
                 (fn [x]
                   (inc (or x page-length)))))

(defn- key-pressed [{:keys [ui-atom options-atom] :as inspector} ^KeyEvent e]
  (let [expand-fn #(when-not (= :atom (::tag (ui/find-component @ui-atom ::cursor)))
                     (toggle-expansion! inspector (cursor inspector)))]
    (condp = (.getKeyCode e)
      KeyEvent/VK_TAB    (expand-fn)
      KeyEvent/VK_ENTER  (if (.isShiftDown e)
                           (move-cursor! inspector :in)
                           (expand-fn))

      KeyEvent/VK_T      (swap-options! inspector update ::tables
                                        (fn [tables]
                                          (let [path   (cursor inspector)
                                                tables (or tables #{})]
                                            (if (get tables path)
                                              (disj tables path)
                                              (conj tables path)))))

      KeyEvent/VK_COMMA  (scroll-seq! inspector (cursor inspector) safe-dec)
      KeyEvent/VK_PERIOD (scroll-seq! inspector (cursor inspector) safe-inc)

      KeyEvent/VK_LEFT   (move-cursor! inspector :left)
      KeyEvent/VK_RIGHT  (let [ui @ui-atom]
                           (if (and (not (expanded? inspector (cursor inspector)))
                                    (not= :atom (::tag (ui/find-component ui ::cursor))))
                             (expand! inspector (cursor inspector))
                             (move-cursor! inspector :right)))
      KeyEvent/VK_UP     (move-cursor! inspector :up)
      KeyEvent/VK_DOWN   (move-cursor! inspector :down)

      KeyEvent/VK_S      (swap-options! inspector update ::show-indexes not)

      KeyEvent/VK_0      (when (is-shortcut-down? e)
                           (swap-options! inspector update ::scale (constantly 1)))
      KeyEvent/VK_EQUALS (if (is-shortcut-down? e)
                           (swap-options! inspector update ::scale #(+ % 0.1))
                           (show-more! inspector (cursor inspector) @options-atom))
      KeyEvent/VK_MINUS  (if (is-shortcut-down? e)
                           (swap-options! inspector update ::scale #(let [s (- % 0.1)] (if (< s 0.6) 0.6 s)))
                           (show-less! inspector (cursor inspector) @options-atom))
      ;;\c (-> tree .getSelectionModel .getSelectionPath .getLastPathComponent pr-str ->clipboard println)
      ;; \0 (reset! font-size default-font-size)
      ;; \= (swap! font-size inc)
      ;; \- (swap! font-size dec)
      nil)))

(defn key-listener [inspector]
  (proxy [KeyListener] []
    (keyPressed [^KeyEvent e] (#'key-pressed inspector e))
    (keyReleased [e])
    (keyTyped [e])))

(defrecord Inspector [data-atom options-atom ui-atom frame])

(defn- make-new-ui [data options]
  (-> (data->ui data [] options)
      (assoc ::ui/x 10 ::ui/y 10)
      ui/layout
      ui/add-absolute-coords))

(defn inspect
  ([data]
   (inspect data {}))
  ([data options]
   (let [data-atom     (atom data)
         options-atom  (atom (merge default-options options))
         ui-atom       (atom (make-new-ui data @options-atom))
         ^JPanel panel (doto (proxy [JPanel] []
                               (paintComponent [^Graphics2D g]
                                 (let [ui @ui-atom
                                       f  (::scale @options-atom)]
                                   (doto g
                                     (.scale f f)
                                     (.setColor ui/color-background)
                                     (.fillRect -2 -2
                                                (+ 2 (.getWidth ^JPanel this))
                                                (+ 2 (.getHeight ^JPanel this))))
                                   (#'paint-cursor ui g)
                                   (ui/paint! ui g)))))
         frame         (doto (JFrame. "Trinket tree inspector")
                         (.add (doto (JScrollPane. panel)
                                 ((fn [sp]
                                    (.setUnitIncrement (.getVerticalScrollBar ^JScrollPane sp) 16)
                                    (.setUnitIncrement (.getHorizontalScrollBar ^JScrollPane sp) 8)))))
                         (.setSize 400 600))
         inspector     (->Inspector data-atom options-atom ui-atom frame)]

     ;;connected atoms
     (add-watch data-atom ::inspector-ui
                (fn [_ _ _ data]
                  (let [{::keys [scale] :as options} @options-atom
                        {::ui/keys [w h] :as new-ui} (make-new-ui data options)]
                    (.setPreferredSize panel (Dimension. (* scale w) (* scale h)))
                    (reset! ui-atom new-ui)
                    (.revalidate panel)
                    (.repaint frame))))
     (add-watch options-atom ::inspector-ui
                (fn [_ _ _ {::keys [scale] :as options}]
                  (let [{::ui/keys [w h] :as new-ui} (make-new-ui @data-atom options)]
                    (.setPreferredSize panel (Dimension. (* scale w) (* scale h)))
                    (reset! ui-atom new-ui)
                    (.revalidate panel)
                    (.repaint frame))))

     ;;listeners
     (doto panel
       (.setBorder (BorderFactory/createEmptyBorder))
       (.addMouseListener (mouse-listener inspector)))
     (doto frame
       (.addKeyListener (key-listener inspector))
       (.setVisible true))

     (reset! last-inspector inspector)

     inspector)))


(comment
  (inspect {:a     10000
            :bbbb  20
            :ccccc "This is a test"})
  (def ins
    (inspect {:a     10000
              :bbbb  {:gg 88
                      :ffff 10}
              :ccccc "This is a test"}))

  (def ins
    (inspect {:a             10000
              :bbbb          {:gg 88
                              :a 10
                              :b 33
                              :g 999
                              :ffff 10}
              :ee            ["this is a vec" 1000 :foo "tt"]
              :list          (list "this is a list" 4000 :foo "tt")
              :set           #{"sets are nice too"
                               "sets are nice 3"
                               "sets are nice 4"}
              {:map "keys"
               :are "handled"
               :as  "well!"} "yay!"
              :ccccc         "This is a test"}))

  (def the-data
    ["foo"
     (concat
      [{:name "Stathis" :surname "Sideris" :activity "coding"}
       {:name "Tom" :surname "Waits" :activity "music"}
       {:name "Adam" :surname "Harris" :activity "music"}
       {:name "Nick" :surname "Nolte" :activity "music"}
       {:name "Cecil" :surname "Adams" :activity "music"}
       {:name "Salvador" :surname "Dali" :activity "music"}
       {:name "Speedy0" :surname "Gonzales0" :activity "music"}
       {:name "Speedy1" :surname "Gonzales1" :activity "music"}
       {:name "Speedy2" :surname "Gonzales2" :activity "music"}
       {:name "Speedy3" :surname "Gonzales3" :activity "music"}
       {:name "Speedy4" :surname "Gonzales4" :activity "music"}
       {:name "Speedy5" :surname "Gonzales5" :activity "music"}
       {:name "Speedy6" :surname "Gonzales6" :activity "music"}
       {:name "Speedy7" :surname "Gonzales7" :activity "music"}
       {:name "Speedy8" :surname "Gonzales8" :activity "music"}
       {:name "Speedy9" :surname "Gonzales9" :activity "music"}
       {:name "Speedy10" :surname "Gonzales10" :activity "music"}
       {:name "Speedy11" :surname "Gonzales11" :activity "music"}
       {:name "Speedy12" :surname "Gonzales12" :activity "music"}
       {:name "Speedy13" :surname "Gonzales13" :activity "music"}
       {:name "Speedy14" :surname "Gonzales14" :activity "music"}
       {:name "Speedy15" :surname "Gonzales15" :activity "music"}
       {:name "Speedy16" :surname "Gonzales16" :activity "music"}
       {:name "Speedy17" :surname "Gonzales17" :activity "music"}
       {:name "Speedy18" :surname "Gonzales18" :activity "music"}
       {:name "Speedy19" :surname "Gonzales19" :activity "music"}])])

  (def the-data
    {:a             10000
     :bbbb          {:gg 88
                     :ffff 10}
     :ee            ["this is a vec" 1000 :foo "tt"]
     :list          (map inc (range 20))
     :code          (line-seq (clojure.java.io/reader "src/trinket/inspector.clj"))
     :set           #{"sets are nice too"
                      "sets are nice 3"
                      "sets are nice 4"}
     {:map "keys"
      :are "handled"
      :as  "well!"} "yay!"
     :ccccc         "This is a test"})

  (def the-data
    {:a             10000
     :bbbb          {:gg 88
                     :ffff 10}
     :ee            ["this is a vec" 1000 :foo "tt"]
     :list          (map inc (range 20))
     :code          (line-seq (clojure.java.io/reader "src/trinket/inspector.clj"))
     :set           #{"sets are nice too"
                      "sets are nice 3"
                      "sets are nice 4"}
     {:map "keys"
      :are "handled"
      :as  "well!"} "yay!"
     :ccccc         "This is a test"})


  (set-data! {:a     10000
              :bbbb  {:gg 88
                      :ffff 10}
              :ccccc "This is a very important test"})

  (set-data! (vec (map #(apply str (repeat % "O")) (range 200))))

  (set-options! ins {::expanded #{[] [2 ::path/val]}
                     ::cursor [2 ::path/val]})

  (set-options! ins {::expanded #{[] [1 ::path/val]}
                     ::cursor   [1 ::path/val 0 ::path/val]})

  (defn- refresh! [{:keys [data-atom] :as inspector}]
    (swap! data-atom identity))

  (refresh! ins)

  (inspect {:a     {:inner1 20
                    :inner2 20
                    :inner3 20
                    :inner4 20
                    :inner5 20
                    :inner6 20}
            :bbbb  {:inner1 20
                    :inner2 20
                    :inner3 20
                    :inner4 20
                    :inner5 20
                    :inner6 20}
            :ccccc "This is a test"})

  (def ins (inspect {:a [0 1 2 3]}))
  (def ins (inspect the-data))
  (def ins (inspect the-data
                    {::scale    2
                     ::cursor   [1]
                     ::expanded #{[] [1]}}))
  (set-data! the-data)

  (-> (ui/find-component @(:ui-atom @last-inspector) ::cursor)
      (dissoc ::ui/children)
      (clojure.pprint/pprint))
  )
