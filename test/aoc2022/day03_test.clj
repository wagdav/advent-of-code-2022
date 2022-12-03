(ns aoc2022.day03-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.day03 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example (str
               "vJrwpWtwJgWrhcsFMMfFFhFp\n"
               "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n"
               "PmmdzqPrVvPwwTWBwg\n"
               "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n"
               "ttgJtRGJQctTZtZT\n"
               "CrZsJsPPZsGzwwsLwLmpwMDw\n"))

(deftest works
  (testing "with example input"
    (is (= 157 (solve-part1 (parse-input example))))
    (is (= 70 (solve-part2 (parse-input example)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day03.txt")))]
      (is (= 7821 (solve-part1 input)))
      (is (= 2752 (solve-part2 input))))))
