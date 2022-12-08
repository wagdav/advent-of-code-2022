(ns aoc2022.day08-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.day08 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example "30373
25512
65332
33549
35390")

(deftest works
  (testing "with example input"
    (is (= 21 (solve-part1 (parse-input example))))
    (is (= 8 (solve-part2 (parse-input example)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day08.txt")))]
      (is (= 1672 (solve-part1 input)))
      (is (= 327180 (solve-part2 input))))))
