(ns aoc2022.day14-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.day14 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(deftest works
  (testing "with example input"
    (is (= 24 (solve-part1 (parse-input example))))
    (is (= 93 (solve-part2 (parse-input example)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day14.txt")))]
        (is (= 1133 (solve-part1 input)))
        (is (= 27566 (solve-part2 input))))))
