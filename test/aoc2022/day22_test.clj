(ns aoc2022.day22-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.day22 :as p1]
            [aoc2022.day22-part2 :as p2]
            [clojure.java.io :as io]))

(deftest works
  (testing "with example input"
    (let [input (slurp (io/resource "day22-example.txt"))]
      (is (= 6032 (p1/solve-part1 (p1/parse-input input))))
      (is (= 5031 (p2/solve-part2 (p2/parse-input input) p2/cube-example)))))

  (testing "with real input"
    (let [input (slurp (io/resource "day22.txt"))]
      (is (= 64256 (p1/solve-part1 (p1/parse-input input))))
      (is (= 109224 (p2/solve-part2 (p2/parse-input input) p2/cube-real))))))
