(ns aoc2022.day03-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.day03 :refer [parse-input solve-part1 solve-part2]]))

(def example-input "")

(deftest works
  (testing "with example input"
    (is (= nil (solve-part1 example-input)))
    (is (= nil (solve-part2 example-input))))

  (testing "with real input"
    (let [input (parse-input "day03.txt")]
      (is (= nil (solve-part1 input)))
      (is (= nil (solve-part2 input))))))