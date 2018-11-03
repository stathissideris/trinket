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

(def current-ui (atom nil))

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

(defn- data->ui [data]
  (if (map? data)
    (let [k->str   (zipmap (keys data)
                           (map pr-str (keys data)))
          longest  (apply max (map count (vals k->str)))
          k->str   (update-vals k->str #(right-pad % longest))
          last-idx (dec (count data))]
      (ui/map->Vertical
       {::ui/x 15 ::ui/y 15
        ::ui/children
        (for [[idx [k v]] (map-indexed vector data)]
          (ui/map->Horizontal
           {::ui/children
            [(if (zero? idx) (ui/text "{") (ui/text " "))
             (ui/text (k->str k)) (ui/text " ") (ui/text (pr-str v))
             (if (= idx last-idx) (ui/text "}") (ui/text " "))]}))}))))

(defn- paint-tree [this g data]
  ;;(.drawLine g 0 0 (.getWidth this) (.getHeight this))
  (ui/paint! @current-ui g))

(defn- tree-inspector
  [data]
  (reset! current-ui (ui/layout (data->ui data)))
  (proxy [JPanel] []
    (paintComponent [g]
      (#'paint-tree this g data))))


(defn ->clipboard [s]
  (-> (Toolkit/getDefaultToolkit)
      .getSystemClipboard
      (.setContents (StringSelection. s) nil))
  s)

(defn mouse-listener []
  (proxy [MouseListener] []
    (mouseClicked [^MouseEvent e]
      (if-let [match (ui/component-at-point {::ui/x (.getX e) ::ui/y (.getY e)}
                                            @current-ui)]
        (println "You clicked on" (pr-str match))))
    (mouseEntered [e])
    (mouseExited [e])
    (mousePressed [e])
    (mouseReleased [e])))

(defn key-listener [^JPanel panel]
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


(defn inspect-tree
  [data]
  (let [^JPanel tree (tree-inspector data)]
    (doto tree
      (.addKeyListener (key-listener tree))
      (.addMouseListener (mouse-listener)))
    (doto (JFrame. "Trinket tree inspector")
      (.add (JScrollPane. tree))
      (.setSize 400 600)
      (.setVisible true))))


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
  (inspect-tree {:a     10000
                 :bbbb  20
                 :ccccc "This is a test"})
  (inspect-tree {:a     {:inner1 20
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
