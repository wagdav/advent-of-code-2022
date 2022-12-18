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

(defn manhattan [p1 p2]
  (->> (map - p1 p2)
       (map #(Math/abs ^long %))
       (apply +)))

(defn shortest-path [cubes start end]
  (search/uniform-cost
    (reify search/Problem
      (actions [_ state]
        (->> (adjacent state)
             (remove cubes)))
      (goal? [_ state]
        (= state end))
      (initial-state [_]
        (assert (nil? (cubes start)) "Should start from an empty cube")
        start)
      (result [_ _ action]
        action)
      (step-cost [_ state action]
        (manhattan state end)))))

(defn weight [cubes]
  (->> cubes
       (map (fn [c]
              (into {c 6} (for [n (adjacent c)] [n -1]))))
       (apply merge-with +)))

(defn weight-outside [cubes]
  (->> cubes
       (map (fn [c]
                (into {c 6} (for [n (adjacent c)]
                              (if (cubes n)
                                [n -1]
                                (if (shortest-path cubes n [0 0 0])
                                  [n 1]
                                  [c 0]))))))
       (apply merge-with +)))

(defn solve-part1 [cubes]
  (->> (weight cubes)
       (vals)
       (filter pos?)
       (apply +)))

(defn solve-part2 [input]
  #_(->> (weight-outside input)
         (vals)
         (filter pos?)
         (apply +)))

(defn viz [coords]
  (let [lava? (set coords)
        sz 20]
    (doseq [z (range sz -1 -1)]
      (println "z=" z)
      (doseq [row (range sz)]
        (doseq [col (range sz)]
          (cond
            (lava? [col row z]) (print "o")
            :else (print ".")))
        (println))
      (println))))

(comment
 (def ex (parse-input example))
 (def real (parse-input (slurp (clojure.java.io/resource "day18.txt"))))
 (adjacent [10 10 10])
 (ex [2 2 2])
 (shortest-path ex [2 2 6] [1 1 1])
 (shortest-path ex [2 2 5] [0 0 1])
 (viz (parse-input (slurp (clojure.java.io/resource "day18.txt"))))
 (viz ex)
 (solve-part2 ex)
 (solve-part2 real))
