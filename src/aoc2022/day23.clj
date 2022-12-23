(ns aoc2022.day23
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (let [lines (str/split-lines input)]
    (set
      (apply concat
        (for [[r row] (map-indexed vector lines)]
          (for [[c v] (map-indexed vector row) :when (= v \#)]
            [r c]))))))

(def dirs {:n  [-1  0]
           :s  [ 1  0]
           :e  [ 0  1]
           :w  [ 0 -1]
           :nw [-1 -1]
           :ne [-1  1]
           :sw [ 1 -1]
           :se [ 1  1]})

(def to-check {:n [:n :ne :nw]
               :s [:s :se :sw]
               :w [:w :nw :sw]
               :e [:e :ne :se]})

(defn look [board p d]
  (board (mapv + p d)))

(defn elves-around? [board p]
  (some (partial look board p) (vals dirs)))

(defn no-elves? [board p ds]
  (->> (map dirs ds)
       (map (partial look board p))
       (every? nil?)))

(defn propose [board p ds]
  (first
    (keep
      (fn [d]
        (when (and (elves-around? board p)
                   (no-elves? board p (to-check d)))
          (mapv + p (dirs d))))
      ds)))

(defn step [board directions]
  (let [proposals (->> (for [e board] [(propose board e (take 4 directions)) e])
                       (group-by first) ; proposal -> [[proposal, elf]]
                       (keep (fn [[_ es]] (when (= 1 (count es)) (first es))))) ; [[proposal, elf]]
        to-remove (set (map second proposals))
        to-add (map first proposals)]
    (->> board (remove to-remove) (into to-add) set)))

(defn simulate [board n]
  (loop [i n
         board board
         directions (cycle [:n :s :w :e])]
    (if (zero? i)
      board
      (recur (dec i)
             (step board (take 4 directions))
             (drop 1 directions)))))

(defn simulate2 [board]
  (loop [i 0
         board board
         directions (cycle [:n :s :w :e])]
    (let [new-board (step board (take 4 directions))]
      (if (= new-board board)
        (inc i)
        (recur (inc i)
               new-board
               (drop 1 directions))))))

(defn bounds [board]
  [(->> board (map first) (apply (juxt min max)))
   (->> board (map second) (apply (juxt min max)))])

(defn viz [board]
  (let [[[rmin rmax] [cmin cmax]] (bounds board)]
    (doseq [r (range rmin (inc rmax))]
      (doseq [c (range cmin (inc cmax))]
        (if (board [r c])
          (print \#)
          (print \.)))
      (println))))

(defn empty-ground [board]
  (let [[[rmin rmax] [cmin cmax]] (bounds board)]
    (-
      (* (- (inc rmax) rmin)
         (- (inc cmax) cmin))
      (count board))))

(defn solve-part1 [input]
  (empty-ground (simulate input 10)))

(defn solve-part2 [input]
  (simulate2 input))
