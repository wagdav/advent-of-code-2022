(ns aoc2022.day16
  (:require [clojure.string :as str]
            [aoc2022.search :as search]))

(def example "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II")

(defn parse-input [input]
  (let [lines (->> (str/split-lines input)
                   (map #(re-seq #"\d+|[A-Z][A-Z]" %))
                   (map (fn [[from rate & to]] [from [(Integer/parseInt rate) (set to)]])))]
    {:rates (into {} (for [[c l] lines] [c (first l)]))
     :tunnels (into {} (for [[c l] lines] [c (second l)]))}))

(defn initial-state [input]
  (merge input
    {:players 1
     :position "AA"
     :remaining 30
     :open? #{}}))

(defn open-valve [state valve]
  (-> state
      (update :open? conj valve)
      (update :remaining dec)))

(defn walk [state dest]
  (-> state
      (assoc :position dest)
      (update :remaining dec)))

(def total-pressure
  (memoize
    (fn [{:keys [remaining position tunnels rates open?] :as state} init player]
      (if (zero? remaining)
        (if (= player 1)
          0
          (total-pressure (assoc (init) :open? open?) init (dec player)))
        (apply max
          (cond->
            (for [dest (tunnels position)]
               (total-pressure (walk state dest) init player))

            ; there's a valve which makes sense to open
            (and (pos? (rates position))
                 (not (open? position)))
            (conj
              (+
                (* (dec remaining) (rates position))
                (total-pressure (open-valve state position) init player)))))))))

(defn solve-part1 [input]
  (total-pressure (initial-state input) initial-state 1))

(defn solve-part2 [input]
  (letfn [(init [] (assoc (initial-state input) :remaining 26))]
    (total-pressure (assoc (initial-state input) :remaining 26) init 2)))

(comment
   (def ex (parse-input example))

   (time (solve-part1 ex)) ; should be 1651
   (time (solve-part2 ex))) ; should be 1707
