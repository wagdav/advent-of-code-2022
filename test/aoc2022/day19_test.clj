(ns aoc2022.day19-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.day19 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example "Blueprint 1:
  Each ore robot costs 4 ore.
  Each clay robot costs 2 ore.
  Each obsidian robot costs 3 ore and 14 clay.
  Each geode robot costs 2 ore and 7 obsidian.

Blueprint 2:
  Each ore robot costs 2 ore.
  Each clay robot costs 3 ore.
  Each obsidian robot costs 3 ore and 8 clay.
  Each geode robot costs 3 ore and 12 obsidian.")

(deftest works
  (testing "with example input"
    (is (= 33 (solve-part1 (parse-input example)))))

  (testing "with real input")
  (let [input (parse-input (slurp (io/resource "day19.txt")))]
    (is (= 1487 (solve-part1 input)))
    (is (= 13440 (solve-part2 input)))))
