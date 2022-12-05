(ns aoc2022.day05-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.day05 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example
  (str
    "    [D]    \n"
    "[N] [C]    \n"
    "[Z] [M] [P]\n"
    " 1   2   3 \n"
    "\n"
    "move 1 from 2 to 1\n"
    "move 3 from 1 to 3\n"
    "move 2 from 2 to 1\n"
    "move 1 from 1 to 2\n"))

(deftest works
  (testing "with example input"
    (is (= "CMZ" (solve-part1 (parse-input example))))
    (is (= "MCD" (solve-part2 (parse-input example)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day05.txt")))]
      (is (= "LBLVVTVLP" (solve-part1 input)))
      (is (= "TPFFBDRJD" (solve-part2 input))))))
