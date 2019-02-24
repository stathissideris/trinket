(ns trinket.print-test
  (:require [trinket.print :refer :all]
            [clojure.test :refer :all])
  (:refer-clojure :exclude [pr-str]))

(deftest pr-str-test
  (testing "vectors"
    (is (= "..." (pr-str [1 [2]] 0)))
    (is (= "[..." (pr-str [1 [2]] 1)))
    (is (= "[1..." (pr-str [1 [2]] 2)))
    (is (= "[1 ..." (pr-str [1 [2]] 3)))
    (is (= "[1 [..." (pr-str [1 [2]] 4)))
    (is (= "[1 [2..." (pr-str [1 [2]] 5)))
    (is (= "[1 [2]..." (pr-str [1 [2]] 6)))
    (is (= "[1 [2]]" (pr-str [1 [2]] 7)))
    (is (= "[1 [2]]" (pr-str [1 [2]] 10))))
  (testing "lists"
    (is (= "..." (pr-str '(1 (2)) 0)))
    (is (= "(..." (pr-str '(1 (2)) 1)))
    (is (= "(1..." (pr-str '(1 (2)) 2)))
    (is (= "(1 ..." (pr-str '(1 (2)) 3)))
    (is (= "(1 (..." (pr-str '(1 (2)) 4)))
    (is (= "(1 (2..." (pr-str '(1 (2)) 5)))
    (is (= "(1 (2)..." (pr-str '(1 (2)) 6)))
    (is (= "(1 (2))" (pr-str '(1 (2)) 7)))
    (is (= "(1 (2))" (pr-str '(1 (2)) 10)))))
