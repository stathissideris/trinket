(ns trinket.print
  (:require [clojure.core :as c])
  (:refer-clojure :exclude [pr-str]))

(defprotocol AsString
  (as-str [x budget]))

(defn pr-str [x budget]
  (let [[s char-left] (as-str x buf budget)]
    (if (>= char-left 0)
      s
      (str s "..."))))

(defn- pr-coll [x budget opening closing]
  (if (zero? budget)
    ["" -1]
    (let [buffer (StringBuffer. opening)]
      (loop [rem    x
             budget (dec budget)]
        (cond (<= budget 0)
              [(.toString buffer) (dec budget)]

              (nil? rem)
              (do (.append buffer closing)
                  [(.toString buffer) (dec budget)])

              :else
              (let [[s new-budget] (as-str (first rem) budget)]
                (.append buffer s)
                (let [n (next rem)]
                  (when (and n (> new-budget 0)) (.append buffer " "))
                  (recur (next rem) (if n
                                      (dec new-budget)
                                      new-budget)))))))))

(extend-protocol AsString
  Object
  (as-str [x budget]
    (let [s (c/pr-str x)]
      [(subs s 0 (min budget (.length s)))
       (- budget (.length s))]))

  clojure.lang.APersistentVector
  (as-str [x budget]) (pr-coll x budget "[" "]")

  clojure.lang.IPersistentList
  (as-str [x budget] (pr-coll x budget "(" ")")))
