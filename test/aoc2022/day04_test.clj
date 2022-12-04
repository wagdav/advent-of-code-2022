(ns aoc2022.day04-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.day04 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example
  (str "2-4,6-8\n"
       "2-3,4-5\n"
       "5-7,7-9\n"
       "2-8,3-7\n"
       "6-6,4-6\n"
       "2-6,4-8"))

(deftest works
  (testing "with example input"
    (is (= 2 (solve-part1 (parse-input example))))
    (is (= 4 (solve-part2 (parse-input example)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day04.txt")))]
      (is (= 602 (solve-part1 input)))
      (is (= 891 (solve-part2 input))))))
