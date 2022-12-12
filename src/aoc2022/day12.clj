(ns aoc2022.day12
  (:require [aoc2022.search :as search]
            [clojure.string :as str]))

(defn parse-input [input]
  (str/split-lines input))

(defn coordinates [heightmap]
  (let [rows (count heightmap)
        cols (count (first heightmap))]
    (for [x (range (inc rows)) y (range (inc cols))] [x y])))

(defn find-start-stop [heightmap]
  (into {}
    (for [pos (coordinates heightmap)
          :let [chr (get-in heightmap pos)]
          :when (#{\S \E} chr)]
      [chr pos])))

(defn height [heightmap pos]
  (when-let [^Character c (get-in heightmap pos)]
    (case c
      \S (int \a)
      \E (int \z)
      (int (Character/toLowerCase c)))))

(defn move [pos direction]
  (mapv + pos direction))

(defn moves [m from]
  (for [dir [[0 1] [0 -1] [1 0] [-1 0]]
        :let [to (move from dir)]
        :when (and (height m to)
                   (<= (- (height m to) (height m from)) 1))]
    dir))

(defn shortest-path [heightmap start end]
  (:path-cost
    (search/uniform-cost
      (reify search/Problem
        (actions [_ state]
          (moves heightmap state))
        (goal? [_ state]
          (= state end))
        (initial-state [_]
          start)
        (result [_ state action]
          (move state action))
        (step-cost [_ _ _]
          1)))))

(defn solve-part1 [input]
  (let [s (find-start-stop input)]
    (shortest-path input (s \S) (s \E))))

(defn solve-part2 [input]
  (let [s (find-start-stop input)
        end (s \E)]
    (->>
      (for [start (coordinates input)
            :when (#{\a \S} (get-in input start))]
        (shortest-path input start end))
      (remove nil?)
      (apply min))))
