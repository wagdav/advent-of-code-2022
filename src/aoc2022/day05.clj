(ns aoc2022.day05
  (:require [clojure.string :as str]))

(defn transpose [m]
  (apply mapv vector m))

(defn parse-input [input]
  (let [[crate-block instr-block] (str/split input #"\R\R")]
    {:crates (zipmap
               (range 1 10)
               (->> (str/split-lines crate-block)
                    (map (fn [line] (->> line (drop 1) (partition 1 4) flatten)))
                    (butlast)
                    (transpose)
                    (map #(remove #{\space} %))))
     :procedure (->> (re-seq #"\d+" instr-block)
                     (map #(Integer/parseInt %))
                     (partition 3))}))

(defn move-by-one [crates [amount from to]]
  (-> crates
    (update from #(drop amount %))
    (update to into (take amount (crates from)))))

(defn move-many [crates [amount from to]]
  (-> crates
    (update from #(drop amount %))
    (update to into (reverse (take amount (crates from))))))

(defn code [crates]
  (str/join
    (for [i (sort (keys crates))]
      (first (crates i)))))

(defn solve-part1 [{:keys [crates procedure]}]
  (code (reduce move-by-one crates procedure)))

(defn solve-part2 [{:keys [crates procedure]}]
  (code (reduce move-many crates procedure)))
