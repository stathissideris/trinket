(ns trinket.inspector
  (:require [trinket.ui :as ui]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.zip :as zip])
  (:import [java.awt Toolkit Graphics2D]
           [java.awt.event KeyListener KeyEvent MouseListener MouseEvent]
           [java.awt.datatransfer StringSelection]
           [javax.swing JPanel JFrame JScrollPane]))

(set! *warn-on-reflection* true)

(defonce current-data (atom nil))
(defonce current-options (atom nil))

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

(defn collection-tag [x]
  (cond
    (map? x)                           :map
    (set? x)                           :set
    (vector? x)                        :vector
    (list? x)                          :list
    (string? x)                        :list
    (instance? clojure.lang.LazySeq x) :lazy-seq
    :else                              :atom))

(defmulti data->ui (fn [data path options] (collection-tag data)))

(defn sequential->ui [data path {::keys [cursor expanded opening closing indent-str] :as options}]
  (let [last-idx (dec (count data))]
    (ui/map->Vertical
     {::ui/x        15 ;;overwritten when it's nested
      ::ui/y        15
      ::cursor      (= cursor path)
      ::ui/children
      (for [[idx v] (map-indexed vector data)]
        (let [value-path (conj path idx)]
          (ui/map->Horizontal
           {::ui/children
            [ ;;opening
             (if (zero? idx)
               (-> (ui/text opening) (assoc ::path path)) ;;assoc path to allow mouse selection of whole map
               (ui/text (or indent-str " ")))

             ;;value
             (if (get expanded value-path)
               (data->ui v value-path options)
               (cond-> (ui/text (pr-str v))
                 :always (assoc ::path value-path)
                 (= cursor value-path) (assoc ::cursor true)))

             ;; closing
             (if (= idx last-idx)
               (-> (ui/text closing) (assoc ::path path))
               (ui/text " "))]})))})))

(defmethod data->ui :vector
  [data path options]
  (sequential->ui data path (assoc options ::opening "[" ::closing "]")))

(defmethod data->ui :list
  [data path options]
  (sequential->ui data path (assoc options ::opening "(" ::closing ")")))

(defmethod data->ui :set
  [data path options]
  (sequential->ui data path (assoc options ::opening "#{" ::closing "}" ::indent-str "  ")))

(defmethod data->ui :map
  [data path {::keys [cursor expanded] :as options}]
  (let [k->str   (zipmap (keys data)
                         (map pr-str (keys data)))
        longest  (apply max (map count (vals k->str)))
        k->str   (update-vals k->str #(right-pad % longest))
        last-idx (dec (count data))]
    (ui/map->Vertical
     {::ui/x        15 ;;overwritten when it's nested
      ::ui/y        15
      ::cursor      (= cursor path)
      ::ui/children
      (for [[idx [k v]] (map-indexed vector data)]
        (let [key-path (conj path k ::key)
              val-path (conj path k ::val)]
          (ui/map->Horizontal
           {::ui/children
            [;;opening
             (if (zero? idx)
               (-> (ui/text "{") (assoc ::path path)) ;;assoc path to allow mouse selection of whole map
               (ui/text " "))

             ;;key
             (if (get expanded key-path)
               (data->ui k key-path options)
               (cond-> (ui/text (k->str k))
                 :always (assoc ::path key-path)
                 (= cursor key-path) (assoc ::cursor true)))

             (ui/text " ")

             ;;value
             (if (get expanded val-path)
               (data->ui v val-path options)
               (cond-> (ui/text (pr-str v))
                 :always (assoc ::path val-path)
                 (= cursor val-path) (assoc ::cursor true)))

             ;; closing
             (if (= idx last-idx)
               (-> (ui/text "}") (assoc ::path path))
               (ui/text " "))]})))})))

(defn paint-cursor [ui ^Graphics2D g]
  (when-let [match (ui/find-component ui ::cursor)]
    (let [cursor (ui/grow-bounds match 1)]
      (doto g
        (.setColor ui/selection-background)
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
  (reset! data-atom data))

(defn- set-options! [{:keys [options-atom] :as inspector} options]
  (reset! options-atom options))

(defn- swap-options! [{:keys [options-atom] :as inspector} & args]
  (apply swap! options-atom args))

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

(defn- move-cursor! [{:keys [options-atom] :as inspector} direction]
  (let [options @options-atom]
    (prn direction (::cursor options))
    (cond (and (= :left direction) (= ::val (last (::cursor options))))
          (swap-options! inspector update ::cursor #(conj (vec (butlast %)) ::key))
          (and (= :right direction) (= ::key (last (::cursor options))))
          (swap-options! inspector update ::cursor #(conj (vec (butlast %)) ::val))
          :else nil)))

(defn- mouse-clicked [{:keys [ui-atom] :as inspector} ^MouseEvent e]
  (when-let [match (ui/component-at-point
                    {::ui/x (.getX e) ::ui/y (.getY e)}
                    @ui-atom)]
    (println "click on:" (pr-str match))
    (condp = (.getClickCount e)
      1 (swap-options! inspector assoc ::cursor (::path match))
      2 (toggle-expansion! inspector (::path match))
      nil)))

(defn mouse-listener [{:keys [ui-atom] :as inspector}]
  (proxy [MouseListener] []
    (mouseClicked [e] (#'mouse-clicked inspector e))
    (mouseEntered [e])
    (mouseExited [e])
    (mousePressed [e])
    (mouseReleased [e])))

(defn- key-pressed [inspector ^KeyEvent e]
  (prn 'KEY e)
  (condp = (.getKeyCode e)
    KeyEvent/VK_ENTER (toggle-expansion! inspector (cursor inspector))
    KeyEvent/VK_LEFT  (move-cursor! inspector :left)
    KeyEvent/VK_RIGHT (move-cursor! inspector :right)
    KeyEvent/VK_UP    (move-cursor! inspector :up)
    KeyEvent/VK_DOWN  (move-cursor! inspector :down)
    ;;\c (-> tree .getSelectionModel .getSelectionPath .getLastPathComponent pr-str ->clipboard println)
    ;; \0 (reset! font-size default-font-size)
    ;; \= (swap! font-size inc)
    ;; \- (swap! font-size dec)
    nil))

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
         options-atom  (atom options)
         ui-atom       (atom (ui/layout (data->ui data [] options)))
         ^JPanel panel (doto (proxy [JPanel] []
                               (paintComponent [g]
                                 (let [ui @ui-atom]
                                   (#'paint-cursor ui g)
                                   (ui/paint! ui g)))))
         frame         (doto (JFrame. "Trinket tree inspector")
                         (.add (JScrollPane. panel))
                         (.setSize 400 600))
         inspector     (->Inspector data-atom options-atom ui-atom frame)]

     ;;connected atoms
     (add-watch data-atom ::inspector-ui
                (fn [_ _ _ data]
                  (swap! ui-atom (fn [_] (ui/layout (data->ui data [] @options-atom))))
                  (.repaint frame)))
     (add-watch options-atom ::inspector-ui
                (fn [_ _ _ options]
                  (swap! ui-atom (fn [_] (ui/layout (data->ui @data-atom [] options))))
                  (.repaint frame)))

     ;;listeners
     (.addMouseListener panel (mouse-listener inspector))
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

  (set-data! ins {:a     10000
                  :bbbb  {:gg 88
                          :ffff 10}
                  :ccccc "This is a very important test"})
  (set-options! ins {::expanded #{[:bbbb ::val]}
                     ::cursor [:bbbb ::val]})
  (set-options! ins {::expanded #{[:bbbb ::val]}
                     ::cursor   [:bbbb ::val :gg ::val]})

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
