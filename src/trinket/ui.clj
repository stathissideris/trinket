(ns trinket.ui
  (:require [clojure.zip :as zip]
            ;;[trinket.perf :as perf]
            )
  (:import [java.awt Graphics2D Color Font Rectangle]
           [javax.swing SwingUtilities JComponent JLabel]))

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

(defmacro later [& exprs]
  `(SwingUtilities/invokeLater (fn [] ~@exprs)))

(defmacro later-val [& exprs]
  `(let [promise# (promise)]
     (SwingUtilities/invokeLater
      (fn []
        (deliver promise# (do ~@exprs))))
     promise#))

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
   (fn [^Font font size]
     (.deriveFont font (float size)))))

(defn- fully-outside-clip? [ax ay w h ^Rectangle clip]
  (let [cx       (.-x clip)
        cy       (.-y clip)
        cw       (.-width clip)
        ch       (.-height clip)
        c-left   (+ cx cw)
        c-bottom (+ cy ch)]
    (or (> ax c-left)
        (> ay c-bottom)
        (< (+ ax w) cx)
        (< (+ ay h) cy))))

;;(def fully-outside-clip? (memoize fully-outside-clip?*))

(defrecord Text []
  Component
  (paint! [{::keys [text ax ay w h selected font size color underline] :as this} g]
    (when-not (perf/acc :clip-check (fully-outside-clip? ax ay w h (.getClipBounds ^Graphics2D g)))
      (perf/acc
       :prepare-label
       (do
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
           (.setForeground text-stamp color-text))))
      (perf/acc
       :paint-at
       (paint-at! text-stamp g this))))
  (ideal-size [this]
    {::w (* 7 (.length (::text this)))
     ::h 15})
  (layout [this]
    (merge this (ideal-size this))))

(defn text [x]
  (cond (string? x)
        (assoc (->Text) ::text x)
        (map? x)
        (merge (->Text) x)
        :else
        (assoc (->Text) ::text (str x))))


(defn safe-max [coll]
  (if (empty? coll)
    0
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

(defn- map-grid [rows columns fun v]
  (let [vv (transient v)]
    (loop [r 0]
      (when-not (= r rows)
        (loop [c 0]
          (when-not (= c columns)
            (let [idx (+ (* r columns) c)]
              (assoc! vv idx (fun r c (get vv idx))))
            (recur (inc c))))
        (recur (inc r))))
    (persistent! vv)))

(defrecord Grid []
  Component
  (paint! [{::keys [children] :as this} g]
    (doseq [child children] (paint! child g)))
  (ideal-size [this]
    (layout this))
  (layout [{::keys [children columns column-padding]
            :or    {column-padding 0}
            :as    this}]
    (let [laid-out      (mapv #(when % (layout %)) children)
          row-count     (int (Math/ceil (/ (count children) columns)))
          column-widths (vec
                         (for [c (range columns)]
                           (safe-max
                            (for [r (range row-count)]
                              (-> laid-out (nth (+ (* r columns) c)) (get ::w 0) (+ column-padding))))))

          x-positions   (vec (reductions + 0 column-widths))

          row-heights   (vec
                         (for [r (range row-count)]
                           (safe-max
                            (for [c (range columns)]
                              (-> laid-out (nth (+ (* r columns) c)) (get ::h 0))))))
          y-positions   (vec (reductions + 0 row-heights))]

      (assoc this
             ::w (apply + column-widths)
             ::h (apply + row-heights)
             ::children
             (map-grid row-count
                       columns
                       (fn [r c child]
                         (when child
                           (let [x (get x-positions c)
                                 y (get y-positions r)
                                 w (get column-widths c)
                                 h (get row-heights r)]
                             (merge child
                                    (position-in-rect child {::x x ::y y ::w w ::h h})))))
                       laid-out)))))

(defn grid [options]
  (map->Grid
   (merge {::layout "grid"} options)))

(defn horizontal [{::keys [children] :as options}]
  (map->Grid
   (merge {::columns (count children)
           ::layout  "horizontal"}
          options)))

(defn vertical [options]
  (map->Grid
   (merge {::columns 1
           ::layout "vertical"}
          options)))

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
                                  (try
                                   (assoc node
                                          ::ax (+ x (::ax parent))
                                          ::ay (+ y (::ay parent)))
                                   (catch Exception e
                                     (throw (ex-info "Error" {:node node} e))))))
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
