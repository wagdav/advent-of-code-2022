(ns aoc2022.day23-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.day23 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example "....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..")

(deftest works
  (testing "with example input"
    (is (= 110 (solve-part1 (parse-input example))))
    (is (= 20 (solve-part2 (parse-input example)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day23.txt")))]
      (is (= 3862 (solve-part1 input)))
      (is (= 913 (solve-part2 input))))))
