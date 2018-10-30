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

(defn ideal-size [^JComponent c]
  (let [ps (.getPreferredSize c)]
    [(.getWidth ps) (.getHeight ps)]))

(defn set-bounds! [^JComponent c [[x y] [w h]]]
  (.setBounds c x y w h))

(defmacro save-transform [g & body]
  `(let [g#  ~g
         tr# (.getTransform ^Graphics2D g#)]
     ~@body
     (.setTransform g# tr#)))

(defn- paint-at
  ([^JComponent what ^Graphics2D g pos]
   (paint-at what g pos (ideal-size what)))
  ([^JComponent what ^Graphics2D g [^int x ^int y :as pos] size]
   (save-transform
    g
    (.validate what)
    (set-bounds! what [pos size])
    (.translate g x y)
    (.paint what g))))

(defn- paint-tree [^JPanel this ^Graphics2D g ^JLabel label]
  ;;(.drawLine g 0 0 (.getWidth this) (.getHeight this))
  (paint-at label g [50 150]))

(defn- tree-inspector
  [data]
  (let [^JLabel label (doto (JLabel.)
                        (.setFont (Font. "Monaco" Font/PLAIN @font-size))
                        (.setText (pr-str data))
                        (.setOpaque true)
                        (.setBackground selection-background))]
   (proxy [JPanel] []
     (paintComponent [^Graphics2D g]
       (#'paint-tree this g label)))))


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
