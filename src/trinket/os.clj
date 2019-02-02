(ns trinket.os
  (:require [clojure.string :as str]))

(defn os []
  (System/getProperty "os.name"))

(defn mac? [] (str/starts-with? (str/lower-case (os)) "mac os x"))
(defn linux? [] (str/starts-with? (str/lower-case (os)) "linux"))
(defn windows? [] (str/starts-with? (str/lower-case (os)) "windows"))
