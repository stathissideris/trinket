(ns trinket.inspector
  (:require [trinket.ui :as ui]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.zip :as zip])
  (:import [java.awt Toolkit]
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
    (instance? clojure.lang.LazySeq x) :lazy-seq
    :else                              :atom))

(defmulti data->ui (fn [data path options] (collection-tag data)))

(defmethod data->ui :map
  [data path {:keys [cursor expanded] :as options}]
  (let [k->str   (zipmap (keys data)
                         (map pr-str (keys data)))
        longest  (apply max (map count (vals k->str)))
        k->str   (update-vals k->str #(right-pad % longest))
        last-idx (dec (count data))]
    (ui/map->Vertical
     {::ui/x 15 ::ui/y 15
      ::ui/children
      (for [[idx [k v]] (map-indexed vector data)]
        (let [key-path (conj path k ::key)
              val-path (conj path k ::val)]
          (ui/map->Horizontal
           {::ui/children
            [(if (zero? idx) (ui/text "{") (ui/text " "))
             (if (get expanded key-path)
               (data->ui k key-path options)
               (-> (ui/text (k->str k))
                   (assoc ::path key-path ::ui/selected (= cursor key-path))))
             (ui/text " ")
             (if (get expanded val-path)
               (data->ui v val-path options)
               (-> (ui/text (pr-str v))
                   (assoc ::path val-path ::ui/selected (= cursor val-path))))
             (if (= idx last-idx) (ui/text "}") (ui/text " "))]})))})))

(defn ->clipboard [s]
  (-> (Toolkit/getDefaultToolkit)
      .getSystemClipboard
      (.setContents (StringSelection. s) nil))
  s)

(defn mouse-listener [ui-atom]
  (proxy [MouseListener] []
    (mouseClicked [^MouseEvent e]
      (if-let [match (ui/component-at-point
                      {::ui/x (.getX e) ::ui/y (.getY e)}
                      @ui-atom)]
        (println "You clicked on" (pr-str match))))
    (mouseEntered [e])
    (mouseExited [e])
    (mousePressed [e])
    (mouseReleased [e])))

(defn key-listener []
  (proxy [KeyListener] []
    (keyPressed [^KeyEvent e]
      (condp = (.getKeyChar e)
        ;;\c (-> tree .getSelectionModel .getSelectionPath .getLastPathComponent pr-str ->clipboard println)
        ;; \0 (reset! font-size default-font-size)
        ;; \= (swap! font-size inc)
        ;; \- (swap! font-size dec)
        nil))
    (keyReleased [e])
    (keyTyped [e])))

(defrecord Inspector [data-atom options-atom ui-atom frame])

(defn- set-data! [{:keys [data-atom] :as inspector} data]
  (reset! data-atom data))

(defn- set-options! [{:keys [options-atom] :as inspector} options]
  (reset! options-atom options))

(defn inspect
  ([data]
   (inspect data {}))
  ([data options]
   (let [data-atom    (atom data)
         options-atom (atom options)
         ui-atom      (atom (ui/layout (data->ui data [] options)))
         ^JPanel tree (doto (proxy [JPanel] []
                              (paintComponent [g]
                                (ui/paint! @ui-atom g)))
                        (.addKeyListener (key-listener))
                        (.addMouseListener (mouse-listener ui-atom)))
         frame        (doto (JFrame. "Trinket tree inspector")
                        (.add (JScrollPane. tree))
                        (.setSize 400 600)
                        (.setVisible true))]

     (add-watch data-atom ::inspector-ui
                (fn [_ _ _ data]
                  (swap! ui-atom (fn [_] (ui/layout (data->ui data [] @options-atom))))
                  (.repaint frame)))

     (add-watch options-atom ::inspector-ui
                (fn [_ _ _ options]
                  (swap! ui-atom (fn [_] (ui/layout (data->ui @data-atom [] options))))
                  (.repaint frame)))

     (->Inspector data-atom options-atom ui-atom frame))))


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

  (set-data! ins {:a     10000
                  :bbbb  {:gg 88
                          :ffff 10}
                  :ccccc "This is a very important test"})
  (set-options! ins {:expanded #{[:bbbb ::val]}
                     :cursor   [:bbbb ::val :gg ::val]})

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
