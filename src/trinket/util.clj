(ns trinket.util)

(defmacro cfuture [& body]
  `(future
     (try
       (let [cl# (.getContextClassLoader (Thread/currentThread))]
         (.setContextClassLoader (Thread/currentThread) (clojure.lang.DynamicClassLoader. cl#)))
       (do ~@body)
       (catch Exception e#
         (.printStackTrace e#)
         (throw e#)))))
