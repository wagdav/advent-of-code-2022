(ns aoc2022.day17-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.day17 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(deftest works
  (testing "with example input"
    (is (= 3068 (solve-part1 (parse-input example))))
    (is (= 1514285714288 (solve-part2 (parse-input example)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day17.txt")))]
        (is (= 3186 (solve-part1 input)))
        (is (= 1566376811584 (solve-part2 input))))))
