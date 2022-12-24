(ns aoc2022.day24-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.day24 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example "#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#")

(deftest works
  (testing "with example input"
    (is (= 18 (solve-part1 (parse-input example))))
    (is (= 54 (solve-part2 (parse-input example)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day24.txt")))]
      (is (= 274 (solve-part1 input)))
      (is (= 839 (solve-part2 input))))))
