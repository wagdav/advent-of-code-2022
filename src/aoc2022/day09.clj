(ns aoc2022.day09
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv #(str/split % #" "))
       (mapv (fn [[d n]] [d (Integer/parseInt n)]))))

(defn expand-move [[row col] [dir amount]]
  (reductions
    (fn [[r c] d]
      (case d
        "U" [(inc r)     c]
        "D" [(dec r)     c]
        "L" [     r (dec c)]
        "R" [     r (inc c)]))
    [row col]
    (repeat amount dir)))

(defn one-step [x]
  (if (pos? x) (min x 1) (max x -1)))

(defn move-tail [tail head]
  (let [delta (map - head tail)]
    (if (every? #(<= % 1) (map abs delta))
      tail
      [(+ (first tail)  (one-step (first delta)))
       (+ (second tail) (one-step (second delta)))])))

(defn move [{:keys [knots visited]} m]
  (let [new-knots (reductions
                    #(reductions move-tail %2 %1)
                    (expand-move (first knots) m)
                    (next knots))]
    {:knots (map last new-knots)
     :visited (into visited (last new-knots))}))

(defn simulate [steps size]
  (reduce move {:knots (repeat size [0 0]) :visited #{}} steps))

(defn solve-part1 [input]
  (count (:visited (simulate input 2))))

(defn solve-part2 [input]
  (count (:visited (simulate input 10))))

(defn viz [size {:keys [knots]}]
  (let [pos->index (zipmap knots (range))]
    (doseq [row (range size (- size) -1)]
      (doseq [col (range (- size) (inc size))]
        (if-let [idx (pos->index [row col])]
          (print (char (+ 48 idx)))
          (print ".")))
      (println))))

(comment
  (viz 5 (simulate [["R" 4] ["U" 4] ["L" 3]] 10)))
