(ns aoc2022.day15-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.day15 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3")

(deftest works
  (testing "with example input"
    (is (= 26 (solve-part1 10 (parse-input example))))
    (is (= 56000011 (solve-part2 20 (parse-input example)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day15.txt")))]
      (is (= 4502208 (solve-part1 2000000 input)))
      (is (= 13784551204480 (solve-part2 4000000 input))))))
