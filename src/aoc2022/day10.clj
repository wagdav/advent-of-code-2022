(ns aoc2022.day10
  (:require [clojure.string :as str]))

(defn parse-input [input]
 (mapv #(str/split % #" ")
       (str/split-lines input)))

(defn cycles [x [instr arg]]
   (case instr
     "noop" [x]
     "addx" [x (+ x (Integer/parseInt arg))]))

(defn run [program]
  (reduce
    (fn [res i]
      (into res (cycles (last res) i)))
    [1]
    program))

(defn solve-part1 [input]
  (let [xs (run input)]
    (apply +
      (for [s [20 60 100 140 180 220]]
        (* s (nth xs (dec s)))))))

(defn solve-part2 [input]
  (let [width 40
        screen (->> (run input)
                    (map-indexed (fn [pos x] (if (<= -1 (- (mod pos width) x) 1)
                                                 \# \space)))
                    (partition width))]
    (doseq [row screen]
      (println (str/join row)))))
