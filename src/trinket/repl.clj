(ns trinket.repl
  (:require [trinket.inspector :as ins]))

(def inspect ins/inspect)
(def inspector ins/inspector)
(def set-data! ins/set-data!)
(def mark! ins/mark!)
(def unmark! ins/unmark!)
(def unmark-all! ins/unmark-all!)
