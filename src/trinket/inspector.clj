(ns trinket.inspector
  (:require [trinket.ui :as ui]
            [trinket.path :as path]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.zip :as zip])
  (:import [java.awt Toolkit Graphics2D]
           [java.awt.event KeyListener KeyEvent MouseListener MouseEvent]
           [java.awt.datatransfer StringSelection]
           [javax.swing JPanel JFrame JScrollPane BorderFactory]))

(set! *warn-on-reflection* true)

(def default-options {::cursor       []
                      ::scale        1
                      ::show-indexes true})

(defn- right-pad [^String s length]
  (str s (apply str (repeat (- length (.length s)) " "))))

(defn- update-vals [m fun]
  (reduce-kv (fn [m k v]
               (assoc m k (fun v)))
             {} m))

(defn- update-keys [m fun]
  (reduce-kv (fn [m k v]
               (assoc m (fun k) v))
             {} m))

(defn- lazy? [x] (instance? clojure.lang.LazySeq x))

(defn collection-tag [x]
  (cond
    (map? x)    :map
    (set? x)    :set
    (vector? x) :vector
    (list? x)   :list
    (string? x) :list
    (lazy? x)   :lazy-seq
    :else       :atom))

(defmulti data->ui (fn [data path options] (collection-tag data)))

(def lazy-indicator
  (ui/text {::ui/text  "L"
            ::ui/size  8
            ::ui/font  ui/font-regular
            ::ui/color ui/color-index}))

(defn- indicate-lazy [ui]
  (ui/map->Horizontal {::ui/children [lazy-indicator ui]}))

(defn atom->ui [{:keys [data text idx last-idx path cursor]}]
  (let [color (cond (keyword? data) ui/color-keywords
                    (string? data)  ui/color-strings
                    :else           ui/color-text)]
    (cond-> (ui/text {::ui/text  (if text text (pr-str data))
                      ::ui/x     10 ;;overwritten when it's nested
                      ::ui/y     10
                      ::ui/color color})
      :always (assoc ::path path
                     ::index idx
                     ::tag (collection-tag data))
      (and idx (zero? idx)) (assoc ::first true)
      (and idx (= idx last-idx)) (assoc ::last true)
      (and cursor (= cursor path)) (assoc ::cursor true)
      (lazy? data) (indicate-lazy))))

(defn sequential->ui [data path {::keys [cursor expanded opening closing indent-str show-indexes idx last-idx] :as options}]
  (if-not (get expanded path)
    (atom->ui {:data data :idx idx :last-idx last-idx :path path :cursor cursor})
    (let [last-idx (dec (count data))]
      (ui/map->Vertical
       {::ui/x        10 ;; overwritten when it's nested
        ::ui/y        10
        ::cursor      (= cursor path)
        ::tag         (collection-tag data)
        ::ui/children
        (for [[idx v] (map-indexed vector data)]
          (let [value-path (conj path idx)]
            (ui/map->Horizontal
             {::ui/children
              [ ;;opening
               (if (zero? idx)
                 (-> (ui/text opening) (assoc ::path path)) ;; assoc path to allow mouse selection of whole map
                 (ui/text (or indent-str " ")))

               ;;value
               (ui/map->Horizontal
                {::ui/children
                 [(when show-indexes (ui/text {::ui/text  (str idx)
                                               ::ui/size  8
                                               ::ui/font  ui/font-regular
                                               ::ui/color ui/color-index}))
                  (if (get expanded value-path)
                    (data->ui v value-path (dissoc options ::indent-str)) ;; no need to inherit this
                    (atom->ui {:data v :idx idx :last-idx last-idx :path value-path :cursor cursor}))]})

               ;; closing
               (if (= idx last-idx)
                 (-> (ui/text closing) (assoc ::path path))
                 (ui/text " "))]})))}))))

(defn lazy->ui [data path {::keys [cursor expanded idx last-idx] :as options}]
  (if-not (get expanded path)
    (indicate-lazy (atom->ui {:data data :idx idx :last-idx last-idx :path path :cursor cursor}))
    (let [last-idx (dec (count data))]
      (indicate-lazy (sequential->ui data path options)))))

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
  (sequential->ui data path
                  (-> options
                      (assoc ::opening "#{" ::closing "}" ::indent-str "  ")
                      (dissoc ::show-indexes))))

(defmethod data->ui :map
  [data path {::keys [cursor expanded idx last-idx] :as options}]
  (let [k->str   (zipmap (keys data)
                         (map pr-str (keys data)))
        longest  (apply max (map count (vals k->str)))
        k->str   (update-vals k->str #(right-pad % longest))
        last-idx (dec (count data))]
    (if-not (get expanded path)
      (atom->ui {:data data :idx idx :last-idx last-idx :path path :cursor cursor})
      (ui/map->Vertical
       {::ui/x        10 ;;overwritten when it's nested
        ::ui/y        10
        ::cursor      (= cursor path)
        ::tag         (collection-tag data)
        ::ui/children
        (for [[idx [k v]] (map-indexed vector data)]
          (let [key-path (conj path idx ::path/key)
                val-path (conj path idx ::path/val)]
            (ui/map->Horizontal
             {::ui/children
              [ ;;opening
               (if (zero? idx)
                 (-> (ui/text "{") (assoc ::path path)) ;;assoc path to allow mouse selection of whole map
                 (ui/text " "))

               ;;key
               (if (get expanded key-path)
                 (data->ui k key-path (assoc options :idx idx :last-idx last-idx))
                 (atom->ui {:data k :text (k->str k) :idx idx :last-idx last-idx :path key-path :cursor cursor}))

               (ui/text " ")

               ;;value
               (if (get expanded val-path)
                 (data->ui v val-path (assoc options :idx idx :last-idx last-idx))
                 (atom->ui {:data v :idx idx :last-idx last-idx :path val-path :cursor cursor}))

               ;; closing
               (if (= idx last-idx)
                 (-> (ui/text "}") (assoc ::path path))
                 (ui/text " "))]})))}))))

(defn paint-cursor [ui ^Graphics2D g]
  (when-let [match (ui/find-component ui ::cursor)]
    (let [cursor (ui/grow-bounds match 1)]
      (doto g
        (.setColor ui/color-selection-background)
        (.fillRect (int (::ui/x cursor))
                   (int (::ui/y cursor))
                   (int (::ui/w cursor))
                   (int (::ui/h cursor)))))))

(defn ->clipboard [s]
  (-> (Toolkit/getDefaultToolkit)
      .getSystemClipboard
      (.setContents (StringSelection. s) nil))
  s)

(defn- set-data! [{:keys [data-atom] :as inspector} data]
  (reset! data-atom data)
  nil) ;;prevent print explosion

(defn- set-options! [{:keys [options-atom] :as inspector} options]
  (reset! options-atom options)
  nil) ;;prevent print explosion

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

(defn- key-pressed [{:keys [ui-atom] :as inspector} ^KeyEvent e]
  (let [expand-fn #(when-not (= :atom (::tag (ui/find-component @ui-atom ::cursor)))
                     (toggle-expansion! inspector (cursor inspector)))]
    (condp = (.getKeyCode e)
      KeyEvent/VK_TAB    (expand-fn)
      KeyEvent/VK_ENTER  (if (.isShiftDown e)
                           (move-cursor! inspector :in)
                           (expand-fn))
      KeyEvent/VK_LEFT   (move-cursor! inspector :left)
      KeyEvent/VK_RIGHT  (let [ui @ui-atom]
                           (if (and (not (expanded? inspector (cursor inspector)))
                                    (not= :atom (::tag (ui/find-component ui ::cursor))))
                             (expand! inspector (cursor inspector))
                             (move-cursor! inspector :right)))
      KeyEvent/VK_UP     (move-cursor! inspector :up)
      KeyEvent/VK_DOWN   (move-cursor! inspector :down)

      KeyEvent/VK_S      (swap-options! inspector update ::show-indexes not)

      KeyEvent/VK_0      (swap-options! inspector update ::scale (constantly 1))
      KeyEvent/VK_EQUALS (swap-options! inspector update ::scale #(+ % 0.1))
      KeyEvent/VK_MINUS  (swap-options! inspector update ::scale #(let [s (- % 0.1)] (if (< s 0.6) 0.6 s)))
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

(defn inspect
  ([data]
   (inspect data {}))
  ([data options]
   (let [data-atom     (atom data)
         options-atom  (atom (merge default-options options))
         ui-atom       (atom (ui/layout (data->ui data [] options)))
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
                         (.add (JScrollPane. panel))
                         (.setSize 400 600))
         inspector     (->Inspector data-atom options-atom ui-atom frame)]

     ;;connected atoms
     (add-watch data-atom ::inspector-ui
                (fn [_ _ _ data]
                  (swap! ui-atom (fn [_] (-> (data->ui data [] @options-atom) ui/layout)))
                  (.repaint frame)))
     (add-watch options-atom ::inspector-ui
                (fn [_ _ _ options]
                  (swap! ui-atom (fn [_] (-> (data->ui data [] @options-atom) ui/layout)))
                  (.repaint frame)))

     ;;listeners
     (doto panel
       (.setBorder (BorderFactory/createEmptyBorder))
       (.addMouseListener (mouse-listener inspector)))
     (doto frame
       (.addKeyListener (key-listener inspector))
       (.setVisible true))

     inspector)))


(comment
  (ui/layout (ui/text "foo"))
  (ui/layout (ui/map->Horizontal
              {::ui/x 15 ::ui/y 15
               ::ui/children [(ui/text "foo")
                              (ui/text "fo")
                              (ui/text "f")]}))
  (ui/layout (ui/map->Vertical
              {::ui/x 15 ::ui/y 15
               ::ui/children [(ui/text "foo")
                              (ui/text "fo")
                              (ui/text "f")]}))
  (ui/layout
   (data->ui {:a     10
              :bbbb  20}))
  (ui/layout
   (data->ui {:a     10
              :bbbb  20
              :ccccc "This is a test"}))
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

  (def ins
    (inspect ["foo"
              {:a             10000
               :bbbb          {:gg 88
                               :ffff 10}
               :ee            ["this is a vec" 1000 :foo "tt"]
               :list          (list "this is a list" 4000 :foo "tt")
               :set           #{"sets are nice too"
                                "sets are nice 3"
                                "sets are nice 4"}
               {:map "keys"
                :are "handled"
                :as  "well!"} "yay!"
               :ccccc         "This is a test"}
              "bar"
              "baz"]))

  (def ins
    (inspect {:a             10000
              :bbbb          {:gg 88
                              :ffff 10}
              :ee            ["this is a vec" 1000 :foo "tt"]
              :list          (map inc (range 10))
              :set           #{"sets are nice too"
                               "sets are nice 3"
                               "sets are nice 4"}
              {:map "keys"
               :are "handled"
               :as  "well!"} "yay!"
              :ccccc         "This is a test"}))


  (set-data! ins {:a     10000
                  :bbbb  {:gg 88
                          :ffff 10}
                  :ccccc "This is a very important test"})

  (set-data! ins (vec (map #(apply str (repeat % "O")) (range 200))))

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
  )
