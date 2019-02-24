(ns trinket.print-test
  (:require [trinket.print :refer :all]
            [clojure.test :refer :all]))

(deftest pr-str-limit-test
  (testing "vectors"
    (is (= "..." (pr-str-limit [1 [2]] 0)))
    (is (= "[..." (pr-str-limit [1 [2]] 1)))
    (is (= "[1..." (pr-str-limit [1 [2]] 2)))
    (is (= "[1 ..." (pr-str-limit [1 [2]] 3)))
    (is (= "[1 [..." (pr-str-limit [1 [2]] 4)))
    (is (= "[1 [2..." (pr-str-limit [1 [2]] 5)))
    (is (= "[1 [2]..." (pr-str-limit [1 [2]] 6)))
    (is (= "[1 [2]]" (pr-str-limit [1 [2]] 7)))
    (is (= "[1 [2]]" (pr-str-limit [1 [2]] 10)))
    (is (= "[100000 [2..." (pr-str-limit [100000 [200000]] 10))))
  (testing "lists"
    (is (= "..." (pr-str-limit '(1 (2)) 0)))
    (is (= "(..." (pr-str-limit '(1 (2)) 1)))
    (is (= "(1..." (pr-str-limit '(1 (2)) 2)))
    (is (= "(1 ..." (pr-str-limit '(1 (2)) 3)))
    (is (= "(1 (..." (pr-str-limit '(1 (2)) 4)))
    (is (= "(1 (2..." (pr-str-limit '(1 (2)) 5)))
    (is (= "(1 (2)..." (pr-str-limit '(1 (2)) 6)))
    (is (= "(1 (2))" (pr-str-limit '(1 (2)) 7)))
    (is (= "(1 (2))" (pr-str-limit '(1 (2)) 10)))))
