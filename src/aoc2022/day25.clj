(ns aoc2022.day25
  (:require [clojure.string :as str]))

(def snafu->values {\2 2, \1 1, \0 0, \- -1, \= -2})
(def values->snafu {2 \2, 1 \1, 0 \0, -1 \-, -2 \=})

(defn snafu->decimal [snafu]
  (long
    (apply +
      (map (fn [s i]
             (* (snafu->values s)
                (Math/pow 5 i)))
           (reverse snafu)
           (range)))))

(defn decimal->snafu [decimal]
  (loop [res '()
         v decimal]
    (if (<= v 2)
      (->> (conj res v)
           (map values->snafu)
           (str/join))
      (let [d (-> v (+ 2) (mod 5) (- 2))]
        (recur (conj res d) (quot (+ 2 v) 5))))))

(defn parse-input [input]
  (str/split-lines input))

(defn solve-part1 [input]
  (->> (map snafu->decimal input)
       (apply +)
       (decimal->snafu)))
