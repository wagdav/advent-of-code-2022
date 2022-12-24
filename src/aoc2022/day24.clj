(ns aoc2022.day24
  (:require [clojure.string :as str]
            [aoc2022.search :as search]))

(defn parse-input [input]
  (let [lines (str/split-lines input)
        rows (count lines)
        cols (count (first lines))]
    {:rows rows
     :cols cols
     :board lines}))

(def directions {\> [0 1] \< [0 -1] \^ [-1 0] \v [1 0]})

(defn move [p d]
  (mapv + p d))

(defn move-blizzard [{:keys [rows cols]} [row col] [dr dc]]
  (let [nr (-> (+ row dr) dec (mod (- rows 2)) inc)
        nc (-> (+ col dc) dec (mod (- cols 2)) inc)]
    [nr nc]))

(def blizzards
  (memoize
    (fn [{:keys [board] :as input} t]
      (set
        (for [[r row] (map-indexed vector board)
              [c col] (map-indexed vector row)
              :when (contains? (set (keys directions)) col)
              :let [delta (mapv #(* t %) (directions col))]]
          (move-blizzard input [r c] delta))))))

(defn shortest-path [{:keys [board] :as input} initial-state goal]
  (search/uniform-cost
    (reify search/Problem
      (initial-state [_]
        initial-state)
      (actions [_ {:keys [position minute]}]
        (let [ds (conj (vals directions) [0 0])]
          (->> (map (partial move position) ds)
               (remove (fn [p] (contains? #{\# nil} (get-in board p))))
               (remove (blizzards input (inc minute))))))
      (goal? [_ {:keys [position]}]
        (= position goal))
      (result [_ state action]
        (-> state
            (assoc :position action)
            (update :minute inc)))
      (step-cost [_ _ _]
        1))))

(def initial-state {:position [0 1] :minute 0})

(defn goal [{:keys [rows cols]}]
  [(dec rows) (- cols 2)])

(defn solve-part1 [input]
  (:path-cost (shortest-path input initial-state (goal input))))

(defn solve-part2 [input]
  (let [g (goal input)
        leg1 (shortest-path input initial-state g)
        leg2 (shortest-path input (:state leg1) (:position initial-state))
        leg3 (shortest-path input (:state leg2) g)]
    (+ (:path-cost leg1)
       (:path-cost leg2)
       (:path-cost leg3))))
