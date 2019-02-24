(ns trinket.print
  (:require [clojure.core :as c]))

(defn hidden-collection [x vector]
  (cond vector   "[...]"
        (map? x) "{...}"
        (set? x) "#{...}"
        :else    "(...)"))

(defprotocol AsString
  (as-str [x budget options]))

(defn pr-str-limit [x budget options]
  (let [[s char-left] (as-str x budget options)]
    (if (>= char-left 0)
      s
      (str s "..."))))

(defn- pr-coll [x budget opening closing]
  (if (zero? budget)
    ["" -1]
    (let [buffer (StringBuffer. opening)]
      (loop [rem    x
             budget (- budget (.length opening))]
        (cond (<= budget 0)
              [(.toString buffer) (dec budget)]

              (nil? rem)
              (do (.append buffer closing)
                  [(.toString buffer) (dec budget)])

              :else
              (let [[s new-budget] (as-str (first rem) budget {})]
                (.append buffer s)
                (let [n (next rem)]
                  (when (and n (> new-budget 0)) (.append buffer " "))
                  (recur (next rem) (if n
                                      (dec new-budget)
                                      new-budget)))))))))

(defn- lazy-map [m]
  (when (not-empty m)
    (let [pair (first m)]
      (lazy-seq (cons (key pair) (cons (val pair) (lazy-map (next m))))))))

(extend-protocol AsString
  nil
  (as-str [_ budget _]
    [(subs "nil" 0 (min budget 3))
     (- budget 3)])

  Object
  (as-str [x budget _]
    (let [s (c/pr-str x)]
      [(subs s 0 (min budget (.length s)))
       (- budget (.length s))]))

  clojure.lang.APersistentVector
  (as-str [x budget _] (pr-coll x budget "[" "]"))

  clojure.lang.ISeq
  (as-str [x budget {::keys [as-vector]}]
    (if as-vector
      (pr-coll x budget "[" "]")
      (pr-coll x budget "(" ")")))

  clojure.lang.APersistentSet
  (as-str [x budget _] (pr-coll x budget "#{" "}"))

  clojure.lang.APersistentMap
  (as-str [x budget _] (pr-coll (lazy-map x) budget "{" "}")))
