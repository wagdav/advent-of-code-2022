(ns aoc2022.day04)

(defn parse-input [input]
  (->> (re-seq #"\d+" input)
       (mapv #(Integer/parseInt %))
       (partition 4)))

(defn fully-overlap? [[x1 x2 y1 y2]]
  (or (<= x1 y1 y2 x2)
      (<= y1 x1 x2 y2)))

(defn no-overlap? [[x1 x2 y1 y2]]
  (or (< x2 y1) (< y2 x1)))

(defn solve-part1 [input]
  (count (filter fully-overlap? input)))

(defn solve-part2 [input]
  (count (remove no-overlap? input)))
