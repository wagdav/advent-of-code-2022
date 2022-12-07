(ns aoc2022.day07-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2022.day07 :refer [parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(def example "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(deftest works
  (testing "with example input"
    (is (= 95437 (solve-part1 (parse-input example))))
    (is (= 24933642 (solve-part2 (parse-input example)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day07.txt")))]
      (is (= 1491614 (solve-part1 input)))
      (is (= 6400111 (solve-part2 input))))))
