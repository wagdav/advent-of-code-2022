(ns aoc2022.day13
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (->> (str/split-lines input)
       (remove #(= "" %))
       (map read-string)))

(defn correct? [left right]
  (cond
    (and (number? left) (number? right))
    (cond
      (< left right) true
      (> left right) false
      (= left right) nil)

    (or (number? left) (number? right))
    (if (number? left)
      (recur [left] right)
      (recur left [right]))

    (and (seq left) (seq right))
    (if-some [res (correct? (first left) (first right))]
      res
      (recur (rest left) (rest right)))

    (and (empty? left) (empty? right))
    nil

    (empty? left)
    true

    (empty? right)
    false))

(def divider-packets #{[[2]] [[6]]})

(defn solve-part1 [input]
  (->> (partition 2 input)
       (map-indexed #(if (apply correct? %2) (inc %1) 0))
       (reduce +)))

(defn solve-part2 [input]
  (->> (into input divider-packets)
       (sort correct?)
       (map-indexed #(if (divider-packets %2) (inc %1) 1))
       (reduce *)))
