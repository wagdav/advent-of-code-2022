(ns aoc2022.day15-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.day15 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example-input "")

(deftest works
  (testing "with example input")
    (is (nil? (solve-part1 example-input)))
    (is (nil? (solve-part2 example-input)))

  (testing "with real input")
    (let [input (parse-input (slurp (io/resource "day15.txt")))]
      (is (nil? (solve-part1 input)))
      (is (nil? (solve-part2 input)))))
