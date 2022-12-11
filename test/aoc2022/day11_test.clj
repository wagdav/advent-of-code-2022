(ns aoc2022.day11-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.day11 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example (slurp (clojure.java.io/resource "day11-example.txt")))

(deftest works
  (testing "with example input"
    (is (= 10605 (solve-part1 (parse-input example))))
    (is (nil? (solve-part2 (parse-input example)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day11.txt")))]
      (is (nil? (solve-part1 input)))
      (is (nil? (solve-part2 input))))))
