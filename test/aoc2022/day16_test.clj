(ns aoc2022.day16-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.day16 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II")

(deftest works
  (testing "with example input"
    (is (= 1651 (solve-part1 (parse-input example))))
    (is (= 1707 (solve-part2 (parse-input example)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day16.txt")))]
      #_(time (is (= 2330 (solve-part1 input))))
      (time (is (nil? (solve-part2 input)))))))
