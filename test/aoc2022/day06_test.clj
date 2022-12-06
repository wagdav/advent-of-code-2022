(ns aoc2022.day06-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.day06 :refer [solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(deftest works
  (testing "with example input"
    (is (= 7 (solve-part1 example)))
    (is (= 19 (solve-part2 example))))

  (testing "with real input"
    (let [input (slurp (io/resource "day06.txt"))]
      (is (= 1361 (solve-part1 input)))
      (is (= 3263 (solve-part2 input))))))
