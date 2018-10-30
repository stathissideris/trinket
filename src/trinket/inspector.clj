(ns trinket.inspector
  (:import
   [java.awt Toolkit BorderLayout Font Color]
   [java.awt.event ActionEvent ActionListener KeyListener]
   [java.awt.datatransfer StringSelection]
   [javax.swing UIManager ImageIcon]
   [javax.swing.tree TreeModel TreeCellRenderer DefaultTreeCellRenderer]
   [javax.swing.table TableModel AbstractTableModel]
   [javax.swing JPanel JTree JTable JScrollPane JFrame JToolBar JButton SwingUtilities JLabel]))

(defn single? [x]
  (not (coll? x)))

(defn collection-tag [x]
  (cond
   (map-entry? x) :entry
   (instance? java.util.Map x) :seqable
   (instance? java.util.Set x) :seqable
   (sequential? x) :seq
   (instance? clojure.lang.Seqable x) :seqable
   :else :single))

(defmulti is-leaf collection-tag)
(defmulti get-child (fn [parent index] (collection-tag parent)))
(defmulti get-child-count collection-tag)

(defmethod is-leaf :default [node]
  (single? node))
(defmethod get-child :default [parent index]
  (nth parent index))
(defmethod get-child-count :default [parent]
  (count parent))

(defmethod is-leaf :entry [e]
  (is-leaf (val e)))
(defmethod get-child :entry [e index]
  (get-child (val e) index))
(defmethod get-child-count :entry [e]
  (count (val e)))

(defmethod is-leaf :seqable [parent]
  false)
(defmethod get-child :seqable [parent index]
  (nth (seq parent) index))
(defmethod get-child-count :seqable [parent]
  (count (seq parent)))

(defn tree-model [data]
  (proxy [TreeModel] []
    (getRoot [] data)
    (addTreeModelListener [treeModelListener])
    (getChild [parent index]
      (get-child parent index))
    (getChildCount [parent]
       (get-child-count parent))
    (isLeaf [node]
      (is-leaf node))
    (valueForPathChanged [path newValue])
    (getIndexOfChild [parent child]
      -1)
    (removeTreeModelListener [treeModelListener])))


(defn open-bracket [x]
  (cond (map? x) "{"
        (set? x) "#{"
        (vector? x) "["
        (list? x) "("
        :else "("))

(defn tree-string [{:keys [value expanded]}]
  (cond (map-entry? value)
        (if expanded
          (str (pr-str (key value)) " " (open-bracket (val value)))
          (str (pr-str (key value)) " " (pr-str (val value))))

        expanded
        (open-bracket value)

        :else
        (pr-str value)))


(def default-font-size 11)
(def font-size (atom default-font-size))

(defn tree-renderer [tree]
  (doto tree
    (.setRowHeight (int (* 2 @font-size)))
    (-> .getUI (.setRightChildIndent (* 0.6 @font-size))))
  (let [label (doto (JLabel.)
                (.setFont (Font. "Monaco" Font/PLAIN @font-size)))]
    (add-watch font-size :renderer
               (fn [_ _ _ size]
                 (.setFont label (Font. "Monaco" Font/PLAIN size))
                 (-> tree .getUI (.setRightChildIndent (* 0.6 size)))
                 (doto tree
                   (.setRowHeight (int (* 2 size)))
                   (.repaint))))
    (proxy [DefaultTreeCellRenderer] []
      (getTreeCellRendererComponent [tree value selected expanded leaf row has-focus]
        (if (or has-focus selected)
          (doto label
            (.setOpaque true)
            (.setBackground (Color/decode "0xb4d9fc")))
          (doto label
            (.setOpaque false)
            (.setBackground (Color/WHITE))))
        (doto label
          (.setText (tree-string {:value    value
                                  :expanded expanded})))))))

(defn ->clipboard [s]
  (-> (Toolkit/getDefaultToolkit)
      .getSystemClipboard
      (.setContents (StringSelection. s) nil))
  s)

(defn key-listener [tree]
  (proxy [KeyListener] []
    (keyPressed [e]
      (condp = (.getKeyChar e)
        \c (-> tree .getSelectionModel .getSelectionPath .getLastPathComponent pr-str ->clipboard println)
        \0 (reset! font-size default-font-size)
        \= (swap! font-size inc)
        \- (swap! font-size dec)
        nil))
    (keyReleased [e])
    (keyTyped [e])))


(defn inspect-tree
  "creates a graphical (Swing) inspector on the supplied hierarchical data"
  {:added "1.0"}
  [data]
  (let [tree (JTree. (tree-model data))]
    (UIManager/put "Tree.expandedIcon" (ImageIcon.))
    (UIManager/put "Tree.collapsedIcon" (ImageIcon.))
    (doto tree
      (.setFont (Font. "Monaco" Font/PLAIN 11))
      (.setCellRenderer (tree-renderer tree))
      (.addKeyListener (key-listener tree)))
    (doto (JFrame. "Clojure Inspector")
      (.add (JScrollPane. tree))
      (.setSize 400 600)
      (.setVisible true))))
