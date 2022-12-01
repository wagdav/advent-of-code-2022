(ns aoc2022.day01-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.day01 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(deftest works
  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day01.txt")))]
      (is (= 66719 (solve-part1 input)))
      (is (= 198551 (solve-part2 input))))))
