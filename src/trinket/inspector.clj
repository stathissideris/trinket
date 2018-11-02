(ns trinket.inspector
  (:import
   [java.awt Graphics2D Toolkit BorderLayout Font Color]
   [java.awt.event ActionEvent ActionListener KeyListener KeyEvent]
   [java.awt.datatransfer StringSelection]
   [javax.swing JComponent ImageIcon]
   [javax.swing.tree TreeModel TreeCellRenderer DefaultTreeCellRenderer]
   [javax.swing.table TableModel AbstractTableModel]
   [javax.swing JPanel JTree JTable JScrollPane JFrame JToolBar JButton SwingUtilities JLabel])
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]))

(set! *print-length* 100)
(set! *warn-on-reflection* true)

(def default-font-size 11)
(def font-size (atom default-font-size))
(def selection-background (Color/decode "0xb4d9fc"))


(defmacro save-transform [g & body]
  `(let [g#  ~g
         tr# (.getTransform ^Graphics2D g#)]
     (let [res# (do ~@body)]
       (.setTransform g# tr#)
       res#)))


(defprotocol Component
  (paint-at [this g bounds])
  (ideal-size [this]))


(defn set-bounds! [^JComponent c {:keys [x y w h]}]
  (.setBounds c x y w h))

(extend-type JComponent
  Component
  (paint-at [this ^Graphics2D g {:keys [^int x ^int y w h] :as bounds}]
    (let [bounds (if (and w h)
                   bounds
                   (merge bounds (ideal-size this)))]
      (save-transform
       g
       ;;(.validate what)
       (set-bounds! this bounds)
       (.translate g x y)
       (.paint this g)
       {:bounds bounds})))
  (ideal-size [this]
    (let [ps (.getPreferredSize this)]
      {:w (.getWidth ps) :h (.getHeight ps)})))

(def ^JLabel text-stamp (doto (JLabel.)
                          (.setFont (Font. "Monaco" Font/PLAIN @font-size))
                          (.setOpaque true)
                          (.setBackground selection-background)
                          ))

(defrecord Text [text selected]
  Component
  (paint-at [this g bounds]
    (.setText text-stamp text)
    (if selected
      (doto text-stamp
        (.setOpaque true)
        (.setBackground selection-background))
      (doto text-stamp
        (.setOpaque false)))
    (merge this (paint-at text-stamp g bounds)))
  (ideal-size [this]
    (.setText text-stamp text)
    (ideal-size text-stamp)))

(defn text [s]
  (->Text s false))

(extend-type String
  Component
  (paint-at [this g bounds]
    (paint-at (text this) g bounds))
  (ideal-size [this]
    (ideal-size (text this))))

(extend-type clojure.lang.Keyword
  Component
  (paint-at [this g bounds]
    (paint-at (text (name this)) g bounds))
  (ideal-size [this]
    (ideal-size (text (name this)))))

(defn right-of [{:keys [x y w]}]
  {:x (+ x (or w 0)) :y y})

(defn linear-arrange [parent children next-fn g {:keys [x y] :as bounds}]
  (let [new-children (transient [])]
    (doall
     (reduce (fn [bounds child]
               (let [updated (paint-at child g (next-fn bounds))]
                 (conj! new-children updated)
                 (:bounds updated)))
             bounds children))
    (merge parent
           {:children (persistent! new-children)
            :bounds   (merge {:x x :y y} (ideal-size parent))})))

(defrecord Horizontal [children]
  Component
  (paint-at [this g bounds]
    (linear-arrange this children right-of g bounds))
  (ideal-size [_]
    (let [ideal-children (map ideal-size children)]
     {:w (apply + (map :w ideal-children))
      :h (apply max (map :h ideal-children))})))

(defn below-of [{:keys [x y h]}]
  {:x x :y (+ y (or h 0))})

(defrecord Vertical [children]
  Component
  (paint-at [this g {:keys [x y] :as bounds}]
    (linear-arrange this children below-of g bounds))
  (ideal-size [_]
    (let [ideal-children (map ideal-size children)]
     {:w (apply max (map :w ideal-children))
      :h (apply + (map :h ideal-children))})))

(defn grow-bounds [{:keys [x y w h]} d]
  {:x (- x d)
   :y (- y d)
   :w (+ w (* 2 d))
   :h (+ h (* 2 d))})

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
     (->Vertical
      (for [[idx [k v]] (map-indexed vector data)]
        (->Horizontal [(if (zero? idx) "{" " ")
                       (k->str k) " " (pr-str v)
                       (if (= idx last-idx) "}" " ")]))))))

(defn- paint-tree [^JPanel this ^Graphics2D g data]
  ;;(.drawLine g 0 0 (.getWidth this) (.getHeight this))
  ;;(paint-at (->Text (pr-str data) false) g {:x 50 :y 50})
  (paint-at (data->ui data nil nil) g {:x 10 :y 10}))

(defn- tree-inspector
  [data]
  (proxy [JPanel] []
    (paintComponent [^Graphics2D g]
      (#'paint-tree this g data))))


(defn ->clipboard [s]
  (-> (Toolkit/getDefaultToolkit)
      .getSystemClipboard
      (.setContents (StringSelection. s) nil))
  s)

(defn key-listener [^JTree tree]
  (proxy [KeyListener] []
    (keyPressed [^KeyEvent e]
      (condp = (.getKeyChar e)
        \c (-> tree .getSelectionModel .getSelectionPath .getLastPathComponent pr-str ->clipboard println)
        \0 (reset! font-size default-font-size)
        \= (swap! font-size inc)
        \- (swap! font-size dec)
        nil))
    (keyReleased [e])
    (keyTyped [e])))


(defn inspect-tree
  [data]
  (let [^JTree tree (tree-inspector data)]
    (doto tree
      (.addKeyListener (key-listener tree)))
    (doto (JFrame. "Trinket tree inspector")
      (.add (JScrollPane. tree))
      (.setSize 400 600)
      (.setVisible true))))


(comment
  (inspect-tree {:a     10
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
