(ns aoc2022.day01
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (let [blocks (str/split input #"\R\R")]
    (->> blocks
      (mapv #(re-seq #"-?\d+" %))
      (mapv #(mapv read-string %)))))

(defn solve-part1 [input]
  (->> input
       (map #(reduce + %))
       (apply max)))

(defn solve-part2 [input]
  (->> input
       (map #(reduce + %))
       (sort >)
       (take 3)
       (reduce +)))
