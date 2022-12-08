(ns aoc2022.day08
  (:require [clojure.string :as str]))

(def example "30373
25512
65332
33549
35390")

(defn parse-input [input]
  (let [size (count (str/split-lines input))]
    {:size size
     :trees (->> (re-seq #"\d" input)
                (mapv #(Integer/parseInt %))
                (partition size)
                (mapv #(vec %)))}))

(defn los [{:keys [size trees]} [row col] direction]
 (case direction
   :up    (for [r (range (dec row) -1 -1)] (get-in trees [r col]))
   :down  (for [r (range (inc row) size)]  (get-in trees [r col]))
   :right (for [c (range (inc col) size)]  (get-in trees [row c]))
   :left  (for [c (range (dec col) -1 -1)] (get-in trees [row c]))))

(defn visible?
 ([grid p]
  (or (visible? grid p :up)
      (visible? grid p :down)
      (visible? grid p :left)
      (visible? grid p :right)))
 ([{:keys [trees] :as grid} p direction]
  (every? #(< % (get-in trees p)) (los grid p direction))))

(defn score
 ([grid p]
  (* (score grid p :up)
     (score grid p :down)
     (score grid p :left)
     (score grid p :right)))
 ([{:keys [trees] :as grid} p direction]
  (let [reference (get-in trees p)]
    (reduce (fn [c t]
              (if (< t reference)
                (inc c)
                (reduced (inc c))))
            0
            (los grid p direction)))))

(defn solve-part1 [{:keys [trees size] :as grid}]
  (count (filter true? (for [r (range size) c (range size)] (visible? grid [r c])))))

(defn solve-part2 [{:keys [trees size] :as grid}]
  (apply max (for [r (range size) c (range size)] (score grid [r c]))))
