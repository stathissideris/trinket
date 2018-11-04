(ns trinket.path)

(defn up [path]
  (if (or (= ::val (last path))
          (= ::key (last path)))
    (vec (drop-last 2 path))
    (vec (butlast path))))

(defn val? [path] (= ::val (last path)))
(defn key? [path] (= ::key (last path)))

(defn point-to-key [path] (conj (vec (butlast path)) ::key))
(defn point-to-val [path] (conj (vec (butlast path)) ::val))
