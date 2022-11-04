(ns aoc2022.day07
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (->> (re-seq #"-?\d+" input)
  (mapv #(Integer/parseInt %))))

(defn solve-part1 [input])

(defn solve-part2 [input])
