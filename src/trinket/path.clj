(ns trinket.path
  (:refer-clojure :exclude [first last]))

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

(defn first [path]
  (if (empty? path)
    path
    (let [last-nth (dec (count path))]
      (if (in-map? path)
        (assoc path (dec last-nth) 0)
        (assoc path last-nth 0)))))

(defn point-to-key [path] (conj (vec (butlast path)) ::key))
(defn point-to-val [path] (conj (vec (butlast path)) ::val))
