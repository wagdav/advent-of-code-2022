(ns aoc2022.day12-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.day12 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(deftest works
  (testing "with example input"
    (is (= 31 (solve-part1 (parse-input example))))
    (is (nil? (solve-part2 (parse-input example)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day12.txt")))]
        (is (= 380 (solve-part1 input)))
        (is (nil? (solve-part2 input))))))
