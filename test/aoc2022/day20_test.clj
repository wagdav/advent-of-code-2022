(ns aoc2022.day20-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.day20 :refer [lookup mix parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example "1
2
-3
3
-2
0
4")

(deftest works
  (testing "with example input"
    (is (= 3 (solve-part1 (parse-input example))))
    (is (= 1623178306 (solve-part2 (parse-input example)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day20.txt")))]
      (is (= 11037 (solve-part1 input)))
      (is (= 3033720253914 (solve-part2 input))))))
