(ns trinket.keys
  (:require [clojure.string :as str]
            [trinket.os :as os])
  (:import [java.awt.event KeyListener KeyEvent]))

(defn- member->kw [m]
  (-> m (str/replace-first "VK_" "") str/lower-case (str/replace "_" "-") keyword))

(defn- generate-code-mapping []
  (->> (clojure.reflect/reflect KeyEvent)
       :members
       (map (comp str :name))
       (filter #(str/starts-with? % "VK_"))
       (map (fn [m] [(eval (symbol (str "KeyEvent/" m))) (member->kw m)]))
       (into {})))

(defmacro def-code-mapping []
  `(def code->kw ~(generate-code-mapping)))

(def-code-mapping)

(if (os/mac?)
  (defn is-shortcut-down? [^KeyEvent e] (.isMetaDown e))
  (defn is-shortcut-down? [^KeyEvent e] (.isAltDown e)))

(defn is-shift-down? [^KeyEvent e] (.isShiftDown e))

(defn- translate-event [^KeyEvent e]
  {:code     (code->kw (.getKeyCode e))
   :shift    (is-shift-down? e)
   :shortcut (is-shortcut-down? e)
   :ctrl     (.isControlDown e)
   :meta     (.isMetaDown e)
   :alt      (.isAltDown e)})


(defn listener [{:keys [pressed released typed]}]
  (let [state (atom {:pressed #{}
                     :history []})]
    {:key-atom     state
     :key-listener (proxy [KeyListener] []
                     (keyPressed [^KeyEvent e]
                       (let [e (translate-event e)]
                         (when pressed (pressed e))
                         (swap! state update :pressed conj e)))
                     (keyReleased [^KeyEvent e]
                       (let [e (translate-event e)]
                         (when released (released e))
                         (swap! state update :pressed disj e)))
                     (keyTyped [^KeyEvent e]
                       (let [e (translate-event e)]
                         (when typed (typed e))
                         (swap! state update :history
                                (fn [his]
                                  (take-last 20 (conj his e)))))))}))


(defn pressed? [state code]
  (some #(= code (:code %)) (:pressed state)))
