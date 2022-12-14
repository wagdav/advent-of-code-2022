(ns aoc2022.day14
  (:require [clojure.string :as str]))

(def sand-source [500 0])

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv #(re-seq #"-?\d+" %))
       (mapv (fn [line] (mapv #(Integer/parseInt %) line)))
       (mapv #(partition 2 %))))

(defn unit [x]
  (if (pos? x) (min 1 x) (max -1 x)))

(defn draw-wall [path]
  (conj
    (apply concat
      (for [[from to] (partition 2 1 path)]
        (let [delta (map (comp unit -) to from)]
          (take-while (partial not= to) (iterate #(map + delta %) from)))))
    (last path)))

(defn initial-state [input]
  (let [walls (->> input (map draw-wall) (apply concat) set)
        [left right] (->> walls (map first) (apply (juxt min max)))
        depth (->> walls (map second) (apply max))]
    {:wall? walls
     :left left
     :right right
     :depth depth
     :sand? #{}
     :floor false}))

(defn add-floor [state]
  (-> state
      (update :depth inc)
      (assoc :floor true)))

(defn fall [{:keys [wall? sand? depth]} [x0 y0]]
  (loop [y y0]
    (if (or (wall? [x0 (inc y)])
            (sand? [x0 (inc y)])
            (>= y depth))
     [x0 y]
     (recur (inc y)))))

(defn roll [{:keys [sand? wall? floor depth]} from]
  (remove
    (fn [[_x y :as p]] (or (wall? p)
                          (sand? p)
                          (and floor (< depth y))))
    (map (partial mapv + from)
         [[-1 1] [1 1]])))

(defn sand [{:keys [floor left right] :as state}]
  (loop [[x _y :as pos] (fall state sand-source)]
    (let [moves (roll state pos)]
      (cond
       (empty? moves)
       (update state :sand? conj pos)

       (and (not floor) (or (< x left) (< right x)))
       (assoc state :outflow true)

       :else
       (recur (fall state (first moves)))))))

(defn units-of-sand [{:keys [sand?]}]
  (count sand?))

(defn simulate-until [pred start]
  (->> (iterate sand start)
       (drop-while (complement pred))
       first))

(defn flowing-into-the-abyss? [{:keys [outflow]}]
  (boolean outflow))

(defn source-is-blocked? [{:keys [sand?]}]
  (sand? sand-source))

(defn solve-part1 [input]
  (->> (initial-state input)
       (simulate-until flowing-into-the-abyss?)
       units-of-sand))

(defn solve-part2 [input]
  (->> (initial-state input)
       add-floor
       (simulate-until source-is-blocked?)
       units-of-sand))

(defn viz [{:keys [left right depth sand? wall?]}]
  (let [[xmin xmax] (->> sand? (map first) (apply (juxt min max)))]
    (doseq [row (range 0 (inc depth))]
      (doseq [col (range (min left xmin) (inc (max xmax right)))]
        (cond
          (wall? [col row]) (print "#")
          (sand? [col row]) (print "o")
          :else (print ".")))
      (println))))

(comment
  (require '[aoc2022.day14-test :refer [example]])
  (-> (iterate sand (initial-state (parse-input example)))
      (nth 24)
      viz)
  (-> (iterate sand (add-floor (initial-state (parse-input example))))
      (nth 93)
      viz))
