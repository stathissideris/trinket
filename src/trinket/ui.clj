(ns trinket.ui
  (:import [java.awt Graphics2D Color Font]
           [javax.swing JComponent JLabel])
  (:require [clojure.zip :as zip]))

(def default-font-size 11)
(def font-size (atom default-font-size))

(def color-selection-background (-> (Color/decode "0x5ab2ec")
                                    .darker
                                    .darker
                                    .darker))
(def color-index Color/GRAY)
(def color-background (Color/decode "0x282c34"))
(def color-text (Color/decode "0xbbc2ce"))
(def color-keywords (Color/decode "0xa8a4de"))
(def color-strings (Color/decode "0x9bbc68")) ;;and namespaces too

(def font-mono (Font. "Monaco" Font/PLAIN default-font-size))
(def font-regular (Font. "Lucinda Grande" Font/PLAIN default-font-size))

(defmacro save-transform [g & body]
  `(let [g#  ~g
         tr# (.getTransform ^Graphics2D g#)]
     (let [res# (do ~@body)]
       (.setTransform g# tr#)
       res#)))

(defprotocol Component
  (paint! [this g])
  (ideal-size [this])
  (layout [this]))

(extend-type Object
  Component
  (paint! [this g]
    (throw (ex-info "paint! not implemented for object" {:object this :type (type this)})))
  (ideal-size [this]
    (throw (ex-info "ideal-size not implemented for object" {:object this :type (type this)})))
  (layout [this]
    (throw (ex-info "layout not implemented for object" {:object this :type (type this)}))))

(extend-type nil
  Component
  (paint! [this g])
  (ideal-size [this] {::w 0 ::h 0})
  (layout [this] this))

(defn set-bounds! [^JComponent c {::keys [x y w h]}]
  (.setBounds c x y w h))

(defn paint-at! [^JComponent component ^Graphics2D g {::keys [^int ax ^int ay w h] :as bounds}]
  (assert ax)
  (assert ay)
  (assert w)
  (assert h)
  (save-transform
   g
   ;;(.validate what)
   (set-bounds! component bounds)
   (.translate g ax ay)
   (.paint component g)
   bounds))

(extend-type JComponent
  Component
  (ideal-size [this]
    (let [ps (.getPreferredSize this)]
      {::w (.getWidth ps) ::h (.getHeight ps)})))

(def ^JLabel text-stamp (doto (JLabel.) (.setFont font-mono)))

(def derive-font
  (memoize
   (fn [font size]
     (.deriveFont font (float size)))))

(defrecord Text []
  Component
  (paint! [{::keys [text selected font size color underline] :as this} g]
    (if underline
      (.setText text-stamp (str "<html><u>" text "</u></html>"))
      (.setText text-stamp text))
    (if selected
      (doto text-stamp
        (.setOpaque true)
        (.setBackground color-selection-background))
      (doto text-stamp
        (.setOpaque false)))
    (-> text-stamp (.setFont (or font font-mono)))
    (when size
      (.setFont text-stamp (-> text-stamp .getFont (derive-font size))))
    (if color
      (.setForeground text-stamp color)
      (.setForeground text-stamp color-text))
    (paint-at! text-stamp g this))
  (ideal-size [this]
    (.setText text-stamp (::text this))
    (ideal-size text-stamp))
  (layout [this]
    (merge this (ideal-size this))))

(defn text [x]
  (cond (string? x)
        (assoc (->Text) ::text x)
        (map? x)
        (merge (->Text) x)
        :else
        (assoc (->Text) ::text (str x))))

(defn right-of [{::keys [x y w]}]
  {::x (+ x (or w 0)) ::y y})

(defn linear-arrange [children next-pos]
  (if (empty? children)
    children
    (let [first-c (-> children first (assoc ::x 0 ::y 0) layout)]
      (reduce (fn [children child]
                (conj children (layout (merge child (next-pos (last children))))))
              [first-c] (rest children)))))

(defrecord Horizontal []
  Component
  (paint! [{::keys [x y children] :as this} g]
    (doseq [c (remove nil? children)] (paint! c g)))
  (ideal-size [this]
    (layout this))
  (layout [{::keys [x y children] :as this}]
    (let [children (remove nil? children)]
      (if (empty? children)
        (assoc this ::w 0 ::h 0)
        (let [new-children (linear-arrange children right-of)]
          (assoc this
                 ::children new-children
                 ::w (apply + (map ::w new-children))
                 ::h (apply max (map ::h new-children))))))))

(defn below-of [{::keys [x y h]}]
  {::x x ::y (+ y (or h 0))})

(defrecord Vertical []
  Component
  (paint! [{::keys [x y children] :as this} g]
    (doseq [c (remove nil? children)] (paint! c g)))
  (ideal-size [this]
    (layout this))
  (layout [{::keys [x y children] :as this}]
    (let [children (remove nil? children)]
      (if (empty? children)
        (assoc this ::w 0 ::h 0)
        (let [new-children (linear-arrange children below-of)]
          (assoc this
                 ::children new-children
                 ::w (apply max (map ::w new-children))
                 ::h (apply + (map ::h new-children))))))))

(defn- transpose [rows]
  (apply map vector rows))

(defn safe-max [coll]
  (if (empty? coll)
    []
    (apply max (remove nil? coll))))

(defn- position-in-rect [{cw ::w
                          ch ::h
                          alignment ::alignment
                          :as component
                          :or {alignment "nw"}}
                         {rx ::x
                          ry ::y
                          rw ::w
                          rh ::h
                          :as rect}]
  (condp = alignment
    "nw" {::x rx ::y ry}
    "sw" {::x rx ::y (- (+ ry rh) ch)}
    "ne" {::x (- (+ rx rw) cw) ::y ry}
    "se" {::x (- (+ rx rw) cw) ::y (- (+ ry rh) ch)}))

(defrecord Grid []
  Component
  (paint! [{::keys [x y children] :as this} g]
    (doseq [child children] (paint! child g)))
  (ideal-size [this]
    (layout this))
  (layout [{::keys [children columns column-padding]
            :or    {column-padding 0}
            :as    this}]
    (let [rows          (for [row (partition-all columns children)]
                          (for [child row]
                            (when child (layout child))))

          column-widths (mapv #(safe-max (->> %
                                              (map (fnil ::w {::w 0}))
                                              (map (partial + column-padding))))
                              (transpose rows))
          x-positions   (vec (reductions + 0 column-widths))

          row-heights   (mapv #(safe-max (map (fnil ::h {::h 0}) %)) rows)
          y-positions   (vec (reductions + 0 row-heights))]

      (assoc this
             ::w (apply + column-widths)
             ::h (apply + row-heights)
             ::children
             (apply concat
              (for [[r-idx row] (map-indexed vector rows)]
                (when row
                  (for [[c-idx child] (map-indexed vector row)]
                    (when child
                      (let [x (get x-positions c-idx)
                            y (get y-positions r-idx)
                            w (get column-widths c-idx)
                            h (get row-heights r-idx)]
                        (merge child
                               (position-in-rect child {::x x ::y y ::w w ::h h}))))))))))))

(defn horizontal [{::keys [children] :as options}]
  (map->Grid
   (merge {::columns (count children)} options)))

(defn vertical [options]
  (map->Grid
   (merge {::columns 1} options)))

(defn grow-bounds [{::keys [ax ay w h]} d]
  {::ax (- ax d)
   ::ay (- ay d)
   ::w  (+ w (* 2 d))
   ::h  (+ h (* 2 d))})

(defn zipper [elem]
  (zip/zipper ::children
              ::children
              #(assoc %1 ::children %2) elem))

(defn add-absolute-coords [ui]
  (let [z (zipper ui)]
    (loop [loc z]
      (cond (zip/end? loc)
            (zip/root loc)

            (nil? (zip/node loc))
            (-> loc zip/next recur)

            :else
            (-> (if-let [parent (some-> loc zip/up zip/node)]
                  (zip/edit loc (fn [{::keys [x y] :as node}]
                                  (assoc node
                                         ::ax (+ x (::ax parent))
                                         ::ay (+ y (::ay parent)))))
                  (zip/edit loc (fn [{::keys [x y] :as node}]
                                  (assoc node
                                         ::ax (or x 0)
                                         ::ay (or y 0)))))
                zip/next
                recur)))))

(defn point-within? [{px ::x py ::y} {::keys [ax ay w h]}]
  (when (and px py ax ay w h)
    (and (<= ax px (+ ax w))
         (<= ay py (+ ay h)))))

(defn component-at-point [point ui]
  (let [z       (zipper ui)
        matches (transient [])] ;;we go on until we find the last one
    (loop [loc z]
      (if (zip/end? loc)
        (last (persistent! matches))
        (do
          (when (point-within? point (zip/node loc))
            (conj! matches (zip/node loc)))
          (recur (zip/next loc)))))))

(defn find-component [ui pred]
  (let [z (zipper ui)]
    (loop [loc z]
      (if (zip/end? loc)
        nil
        (if (pred (zip/node loc))
          (zip/node loc)
          (recur (zip/next loc)))))))

(defn scale [ui factor]
  (assert factor)
  (let [z (zipper ui)]
    (loop [loc z]
      (if (zip/end? loc)
        (zip/root loc)
        (let [{::keys [ax ay w h size] :as node :or {size default-font-size}} (zip/node loc)]
          (->> (cond-> node
                 ax (assoc ::ax (* factor ax))
                 ay (assoc ::ay (* factor ay))
                 w (assoc ::w (* factor w))
                 h (assoc ::h (* factor h))
                 size (assoc ::size (* factor size)))
               (zip/replace loc)
               zip/next
               recur))))))
