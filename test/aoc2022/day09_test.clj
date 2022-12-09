(ns aoc2022.day09-test
  (:require [clojure.test :refer [deftest are is testing]]
            [aoc2022.day09 :refer [move-tail parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(deftest works
  (testing "tail movement"
    (are [from head,   to] (= to (move-tail from head))
         ; doesn't move
         [0 0] [0  0], [0 0]
         [0 0] [0  1], [0 0]
         ; moves horizontally
         [0 0] [0  2], [0  1]
         [0 0] [0 -2], [0 -1]
         ; moves diagonally
         [0 0] [1  2], [1  1]
         [0 0] [1 -2], [1 -1]))

  (testing "with example input"
    (is (= 13 (solve-part1 (parse-input example)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day09.txt")))]
      (is (= 5695 (solve-part1 input)))
      (is (= 2434 (solve-part2 input))))))
