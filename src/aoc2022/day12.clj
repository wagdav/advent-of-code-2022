(ns aoc2022.day12
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]))

(def example "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(defn parse-input [input]
  (str/split-lines input))

(def ex (parse-input example))

(defn at [heighthmap [x y]]
  (get-in heighthmap [x y]))

(defn find-start-stop [heightmap]
  (into {}
    (let [rows (count heightmap)
          cols (count (first heightmap))]
      (for [x (range (inc rows))
            y (range (inc cols))
            :when (#{\S \E} (at heightmap [x y]))]
        [(at heightmap [x y]) [x y]]))))

(defn height->int [h]
  (case h
    \S (height->int \a)
    \E (height->int \z)
    (int (first (str/lower-case h)))))

(defn moves [heightmap from]
  (let [directions '([0 1] [0 -1] [1 0] [-1 0])]
    (filter
      (fn [to]
        (and (at heightmap to)
             (>= 1
              (- (height->int (at heightmap to))
                 (height->int (at heightmap from))))))
      (for [d directions]
        (mapv + from d)))))

(defn shortest-path [heightmap start end]
  (loop [frontier (priority-map start 0)
         explored #{}]
    (let [[current distance] (peek frontier)]
      (if (= current end)
        distance
        (recur
          (reduce
             (fn [queue [point tentative-distance]]
               (assoc queue point (min tentative-distance
                                       (get queue point tentative-distance))))

             (pop frontier)

             (->> (moves heightmap current)
                  (remove explored)
                  (map #(vector % (inc distance)))))

          (conj explored current))))))

(defn solve-part1 [input]
  (let [s (find-start-stop input)]
    (shortest-path input (s \S) (s \E))))

(defn solve-part2 [input])
