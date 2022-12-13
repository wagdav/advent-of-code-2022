(ns aoc2022.day13-test
  (:require [clojure.test :refer [are deftest is testing]]
            [aoc2022.day13 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")

(deftest works
  (testing "with example input"
    (is (= 13 (solve-part1 (parse-input example))))
    (is (= 140 (solve-part2 (parse-input example)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day13.txt")))]
      (is (= 5529 (solve-part1 input)))
      (is (= 27690 (solve-part2 input))))))
