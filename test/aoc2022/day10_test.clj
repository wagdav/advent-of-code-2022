(ns aoc2022.day10-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.day10 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(deftest works
  (testing "with example input"
    (let [input (parse-input (slurp (io/resource "day10-example.txt")))]
      (is (= 13140 (solve-part1 input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day10.txt")))]
      (is (= 11780 (solve-part1 input)))
      (is (nil? (solve-part2 input))))))
