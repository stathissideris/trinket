(ns dev
  (:require [trinket.inspector :as ins]
            [trinket.ui :as ui]))

(defn current-component []
  (let [ui  (-> ins/last-inspector deref :ui-atom deref)
        cur (@#'ins/cursor @ins/last-inspector)]
    (ui/find-component ui #(= cur (::ins/path %)))))

(defn options []
  (-> ins/last-inspector deref :options-atom deref))
