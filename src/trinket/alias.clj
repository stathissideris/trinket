(ns trinket.alias
  (:require [clojure.string :as str]))

(defn- make-substring-alias [k x]
  (as-> k $
    (str/split $ #"\.")
    (last $)
    (take x $)
    (apply str $)))

(defn- make-substring-aliases [nss]
  (loop [x               1
         last-candidates nil]
    (let [aliases (map #(make-substring-alias % x) nss)]
      (cond (= aliases last-candidates)
            nil

            (apply distinct? aliases)
            (zipmap nss aliases)

            :else
            (recur (inc x) aliases)))))

(defn- make-fragment-aliases [nss]
  (let [split (map #(str/split % #"\.") nss)]
    (loop [x 1]
      (let [aliases (map (partial take-last x) split)]
        (if (apply distinct? aliases)
          (zipmap nss (map (partial str/join ".") aliases))
          (recur (inc x))))))) ;;TODO constrain somehow

(defn make-aliases [nss]
  (when (seq nss)
    (or (make-substring-aliases nss)
        (make-fragment-aliases nss))))

(defn shorten [k aliases]
  (if (string? k)
    k
    (if-let [a (get aliases (namespace k))]
      (str "::" a "/" (name k))
      (str k))))
