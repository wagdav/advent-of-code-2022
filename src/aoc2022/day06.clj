(ns aoc2022.day06)

(defn start-of-unique [n input]
  (->> (partition n 1 input)
       (map-indexed #(vector %1 (distinct %2)))
       (drop-while #(not= n (count (second %))))
       (ffirst)
       (+ n)))

(defn solve-part1 [input]
  (start-of-unique 4 input))

(defn solve-part2 [input]
  (start-of-unique 14 input))
