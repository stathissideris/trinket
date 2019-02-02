(ns trinket.repl
  (:require [trinket.inspector :as ins]))

(defmacro def-with-doc [nom var-sym]
  (let [doc      (-> var-sym resolve meta :doc)
        arglists (-> var-sym resolve meta :arglists)]
    `(do
       ~(if doc
          `(def ~nom ~doc ~(-> var-sym resolve deref))
          `(def ~nom ~(-> var-sym resolve deref)))
       ~(when arglists
          `(alter-meta! (var ~nom) assoc :arglists (quote ~arglists)))
       ~(resolve nom))))

(def-with-doc inspect ins/inspect)
(def-with-doc inspector ins/inspector)
(def-with-doc set-data! ins/set-data!)
(def-with-doc mark! ins/mark!)
(def-with-doc unmark! ins/unmark!)
(def-with-doc unmark-all! ins/unmark-all!)
