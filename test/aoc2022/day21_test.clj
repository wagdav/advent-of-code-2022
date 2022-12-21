(ns aoc2022.day21-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.day21 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32")

(deftest works
  (testing "with example input"
    (is (= 152 (solve-part1 (parse-input example))))
    (is (= 301 (solve-part2 (parse-input example)))))

  (testing "with real input"
    (time (let [input (parse-input (slurp (io/resource "day21.txt")))]
            (is (= 84244467642604 (solve-part1 input)))
            (is (= 3759569926192 (solve-part2 input)))))))
