(ns aoc2022.day18
  (:require [aoc2022.search :as search]))

(defn parse-input [input]
  (->> (re-seq #"-?\d+" input)
       (mapv #(Integer/parseInt %))
       (partition 3)
       (mapv vec)
       set))

(def faces [[ 1  0  0]
            [-1  0  0]
            [ 0  1  0]
            [ 0 -1  0]
            [ 0  0  1]
            [ 0  0 -1]])

(defn add [x y]
  (mapv + x y))

(defn adjacent [p]
  (map (partial add p) faces))

(defn shortest-path [cubes start end]
  (search/uniform-cost
    (reify search/Problem
      (actions [_ state]
        (->> (adjacent state)
             (remove cubes)))
      (goal? [_ [x y]]
        (or (= [x y] end)
            (not (<= 1 x 20))
            (not (<= 1 y 20))))
      (initial-state [_]
        start)
      (result [_ _ action]
        action)
      (step-cost [_ _state _action]
       1))))

(defn surface-area [cubes]
  (->> cubes
       (map (fn [c]
              (into {c 6} (for [n (adjacent c)] [n -1]))))
       (apply merge-with +)
       (vals)
       (filter pos?)
       (apply +)))

(defn exterior? [cubes p]
  (shortest-path cubes p [0 0 0]))

(defn exterior-surface-area [cubes]
  (->> cubes
       (mapcat (fn [c] (for [n (adjacent c)
                             :when (and (not (cubes n))
                                        (exterior? cubes n))] n)))
       count))

(defn solve-part1 [input]
  (surface-area input))

(defn solve-part2 [input]
  (exterior-surface-area input))
