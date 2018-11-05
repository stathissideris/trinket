(ns trinket.ui
  (:import [java.awt Graphics2D Color Font]
           [javax.swing JComponent JLabel])
  (:require [clojure.zip :as zip]))

(def default-font-size 11)
(def font-size (atom default-font-size))

(def selection-background (Color/decode "0xb4d9fc"))

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

(defn set-bounds! [^JComponent c {::keys [x y w h]}]
  (.setBounds c x y w h))

(defn paint-at! [^JComponent component ^Graphics2D g {::keys [^int x ^int y w h] :as bounds}]
  (assert x)
  (assert y)
  (assert w)
  (assert h)
  (save-transform
   g
   ;;(.validate what)
   (set-bounds! component bounds)
   (.translate g x y)
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
  (paint! [{::keys [text selected font size] :as this} g]
    (.setText text-stamp text)
    (if selected
      (doto text-stamp
        (.setOpaque true)
        (.setBackground selection-background))
      (doto text-stamp
        (.setOpaque false)))
    (-> text-stamp (.setFont (or font font-mono)))
    (when size
      (.setFont text-stamp (-> text-stamp .getFont (derive-font size))))
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

(defn linear-arrange [children {::keys [x y] :as parent} next-pos]
  (let [first-c (-> children first (assoc ::x x ::y y) layout)]
    (reduce (fn [children child]
              (conj children (layout (merge child (next-pos (last children))))))
            [first-c] (rest children))))

(defrecord Horizontal []
  Component
  (paint! [this g]
    (doseq [c (remove nil? (::children this))] (paint! c g)))
  (ideal-size [this]
    (layout this))
  (layout [{::keys [x y children] :as this}]
    (let [new-children (linear-arrange (remove nil? children) this right-of)]
      (assoc this
             ::children new-children
             ::w (apply + (map ::w new-children))
             ::h (apply max (map ::h new-children))))))

(defn below-of [{::keys [x y h]}]
  {::x x ::y (+ y (or h 0))})

(defrecord Vertical []
  Component
  (paint! [this g]
    (doseq [c (remove nil? (::children this))] (paint! c g)))
  (ideal-size [this]
    (layout this))
  (layout [{::keys [x y children] :as this}]
    (let [new-children (linear-arrange (remove nil? children) this below-of)]
      (assoc this
             ::children new-children
             ::w (apply max (map ::w new-children))
             ::h (apply + (map ::h new-children))))))

(defn grow-bounds [{::keys [x y w h]} d]
  {::x (- x d)
   ::y (- y d)
   ::w (+ w (* 2 d))
   ::h (+ h (* 2 d))})

(defn zipper [elem]
  (zip/zipper ::children ::children #(assoc %1 ::children %2) elem))

(defn point-within? [{px ::x py ::y} {::keys [x y w h]}]
  (and (<= x px (+ x w))
       (<= y py (+ y h))))

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
