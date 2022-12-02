(ns aoc2022.day02-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.day02 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example-input "A Y\nB X\n C Z")

(deftest works
  (testing "with example input"
    (is (= 15 (solve-part1 (parse-input example-input))))
    (is (= 12 (solve-part2 (parse-input example-input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day02.txt")))]
      (is (= 13809 (solve-part1 input)))
      (is (= 12316 (solve-part2 input))))))
