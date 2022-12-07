(ns aoc2022.day03
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-input [input]
  (str/split-lines input))

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(def priority
  (zipmap
    (concat (char-range \a \z)
            (char-range \A \Z))
    (range 1 53)))

(defn solve-part1 [input]
  (apply +
         (map
           (fn [line] (->> line
                           (split-at (quot (count line) 2))
                           (map set)
                           (apply set/intersection)
                           (first)
                           (priority)))
           input)))

(defn solve-part2 [input]
  (->> input
       (map set)
       (partition 3)
       (map #(first (apply set/intersection %)))
       (map priority)
       (apply +)))
