(ns trinket.inspector
  (:import
   [java.awt Graphics2D Toolkit BorderLayout Font Color]
   [java.awt.event ActionEvent ActionListener KeyListener KeyEvent]
   [java.awt.datatransfer StringSelection]
   [javax.swing JComponent ImageIcon]
   [javax.swing.tree TreeModel TreeCellRenderer DefaultTreeCellRenderer]
   [javax.swing.table TableModel AbstractTableModel]
   [javax.swing JPanel JTree JTable JScrollPane JFrame JToolBar JButton SwingUtilities JLabel]))

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
       bounds)))
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
  (paint-at [_ g bounds]
    (.setText text-stamp text)
    (if selected
      (doto text-stamp
        (.setOpaque true)
        (.setBackground selection-background))
      (doto text-stamp
        (.setOpaque false)))
    (paint-at text-stamp g bounds))
  (ideal-size [_]
    (.setText text-stamp text)
    (ideal-size text-stamp)))

(defn text [s]
  (->Text s false))

(defn right-of [{:keys [x y w]}]
  {:x (+ x (or w 0)) :y y})

(defrecord Horizontal [children]
  Component
  (paint-at [this g {:keys [x y] :as bounds}]
    (doall
     (reduce (fn [bounds child]
               (paint-at child g (right-of bounds)))
             bounds children))
    (merge {:x x :y y} (ideal-size this)))
  (ideal-size [_]
    (let [ideal-children (map ideal-size children)]
     {:w (apply + (map :w ideal-children))
      :h (apply max (map :h ideal-children))})))

(defn below-of [{:keys [x y h]}]
  {:x x :y (+ y (or h 0))})

(defrecord Vertical [children]
  Component
  (paint-at [this g {:keys [x y] :as bounds}]
    (doall
     (reduce (fn [bounds child]
               (paint-at child g (below-of bounds)))
             bounds children))
    (merge {:x x :y y} (ideal-size this)))
  (ideal-size [_]
    (let [ideal-children (map ideal-size children)]
     {:w (apply max (map :w ideal-children))
      :h (apply + (map :h ideal-children))})))

(defn grow-bounds [{:keys [x y w h]} d]
  {:x (- x d)
   :y (- y d)
   :w (+ w (* 2 d))
   :h (+ h (* 2 d))})

(defn- paint-tree [^JPanel this ^Graphics2D g data]
  ;;(.drawLine g 0 0 (.getWidth this) (.getHeight this))
  (paint-at (->Text (pr-str data) false) g {:x 50 :y 50})
  (paint-at
   (->Vertical
    [(->Horizontal [(text "{")
                    (text ":a 60")])
     (->Horizontal [(text " ")
                    (->Text ":abbbbb 1000" true)])
     (->Horizontal [(text " ")
                    (text ":abbbbb 500")
                    (text "}")])])
   g {:x 0 :y 0}))

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
