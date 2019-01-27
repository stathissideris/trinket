(ns trinket.path
  (:refer-clojure :exclude [first get-in]))

(defn val? [path] (= ::val (clojure.core/last path)))
(defn key? [path] (= ::key (clojure.core/last path)))
(defn in-map? [path] (or (key? path) (val? path)))

(defn up [path]
  (if (empty? path)
    path
    (if (in-map? path)
      (vec (drop-last 2 path))
      (vec (butlast path)))))

(defn- dec-or-zero [x]
  (max 0 (dec x)))

(defn left [path]
  (if (empty? path)
    path
    (let [last-nth (dec (count path))]
      (if (in-map? path)
        (update path (dec last-nth) dec-or-zero)
        (update path last-nth dec-or-zero)))))

(defn right [path]
  (if (empty? path)
    path
    (let [last-nth (dec (count path))]
      (if (in-map? path)
        (update path (dec last-nth) inc)
        (update path last-nth inc)))))

(defn next-row [path]
  (if (empty? path)
    path
    (let [[row col] (take-last 2 path)]
      (vec (concat (drop-last 2 path) [(inc row) col])))))

(defn previous-row [path]
  (if (empty? path)
    path
    (let [[row col] (take-last 2 path)]
      (vec (concat (drop-last 2 path) [(dec-or-zero row) col])))))

(defn first [path]
  (if (empty? path)
    path
    (let [last-nth (dec (count path))]
      (if (in-map? path)
        (assoc path (dec last-nth) 0)
        (assoc path last-nth 0)))))

(defn point-to-key [path] (conj (vec (butlast path)) ::key))
(defn point-to-val [path] (conj (vec (butlast path)) ::val))

(defn offset [path x]
  (if (number? (last path))
    (vec (concat (butlast path) [(+ x (last path))]))
    path))

(defn get-in [data path]
  (let [sentinel (Object.)]
    (loop [m  data
           ks (seq path)]
      (if ks
        (let [k (clojure.core/first ks)]
          (cond (number? k)
                (let [m (nth (seq m) k sentinel)]
                  (if (identical? sentinel m)
                    nil
                    (recur m (next ks))))

                :else
                (if (= ::key k)
                  (recur (key m) (next ks))
                  (recur (val m) (next ks)))))
        m))))
