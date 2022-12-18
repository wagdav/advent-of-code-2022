(ns aoc2022.day18-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.day18 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5")

(deftest works
  (testing "with example input"
    (is (= 64 (solve-part1 (parse-input example))))
    (is (= 58 (solve-part2 (parse-input example)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day18.txt")))]
      (is (= 3396 (solve-part1 input)))
      (is (nil? (solve-part2 input))))))  ; 3252, 2847, 2698 too high
