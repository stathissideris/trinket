(ns trinket.perf
  (:refer-clojure :exclude [time]))

(def enabled false)

(def stats (atom {}))

(defmacro time
  ([tag expr]
   (if-not enabled
     expr
     `(let [tag#   ~(str (if (keyword? tag)
                           (name tag)
                           tag)
                         " ")
            start# (. System (nanoTime))
            ret#   ~expr]
        (println (str (or tag# "Elapsed time: ") (/ (double (- (. System (nanoTime)) start#)) 1000000.0) " msecs"))
        ret#))))

(defmacro acc
  ([tag expr]
   (if-not enabled
     expr
     `(let [tag#   ~tag
            start# (. System (nanoTime))
            ret#   ~expr]
        (swap! stats
               (fn [stats#]
                 (-> stats#
                     (update-in [tag# :time] (fnil + 0)
                                (/ (double (- (. System (nanoTime)) start#)) 1000000.0))
                     (update-in [tag# :count] (fnil inc 0)))))
        ret#))))

(defn checkpoint []
  (when enabled
    (doseq [[tag {:keys [time count]}] @stats]
      (println (str (name tag) " " time " msecs (" count " times)")))
    (println "--")
    (reset! stats {})))
