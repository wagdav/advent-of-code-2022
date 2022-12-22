(ns aoc2022.day22
  (:require [clojure.string :as str]))

(defn pad [v len]
  (if (= len (count v))
    v
    (into v (repeat (- len (count v)) \space))))

(defn parse-path [line]
  (map (fn [p]
         (if (#{"L" "R"} p) p (Integer/parseInt p)))
       (re-seq #"\d+|[RL]" line)))

(defn parse-board [lines]
  (let [width (apply max (map count lines))
        height (count lines)]
    {:width width
     :height height
     :board
      (->> lines
           (mapv vec)
           (mapv #(pad % width)))}))

(defn partitionv [n s]
  (map vec (partition n s)))

(defn parse-input [input]
  (let [[board path] (str/split input #"\R\R")]
    (merge
      (parse-board (str/split-lines board))
      {:path (parse-path path)})))

(defn- set-current [{:keys [facing position] :as state}]
  (let [[row col] position]
    (assoc-in state [:board (dec row) (dec col)]
                    ({0 ">" 1 "v" 2 "<" 3 "^"} facing))))

(defn pos [{:keys [board width height]} [row col]]
  (:pre [(pos? row) (pos? col)])
  (get-in board [(mod (dec row) height)
                 (mod (dec col) width)]))

(def facing {0 :right 1 :down 2 :left 3 :up})
(def directions {0 [0 1] 1 [1 0] 2 [0 -1] 3 [-1 0]})

(defn ahead
  "Return the map-coordinate of the point ahead."
  [{:keys [facing width height] :as state} p]
  (loop [[r c :as ahead] (mapv + p (directions facing))]
    (if (= (pos state ahead) \space)
      (recur (mapv + ahead (directions facing)))
      [(-> r dec (mod height) inc)
       (-> c dec (mod width) inc)])))

(defn advance [{:keys [position] :as state} steps]
  (assoc state :position
    (loop [i steps p position]
      (let [next-p (ahead state p)]
        (if (or (zero? i) (= (pos state next-p) \#))
          p
          (recur (dec i) next-p))))))

(defn turn [state d]
  (update state :facing
    #(mod (case d
           "R" (inc %)
           "L" (dec %))
      4)))

(defn initial-state [state]
  (-> state
      (merge {:facing 0
              :position [1 1]})
      (advance 1)))

(defn password [{:keys [position facing]}]
  (let [[row col] position]
    (+ (* 1000 row)
       (* 4 col)
       facing)))

(defn viz [state]
  (let [{:keys [board]} (set-current state)]
    (doseq [row board]
      (doseq [col row]
          (print col))
      (println))
    state))

(defn solve-part1 [input]
  (password
    (reduce
      (fn [state m]
        (if (number? m)
          (advance state m)
          (turn state m)))
      (initial-state input)
      (:path input))))
