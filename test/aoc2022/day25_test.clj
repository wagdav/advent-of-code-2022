(ns aoc2022.day25-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.day25 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example "1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122")

(deftest works
  (testing "with example input"
    (is (= "2=-1=0" (solve-part1 (parse-input example)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day25.txt")))]
      (is (= "2-02===-21---2002==0" (solve-part1 input))))))
