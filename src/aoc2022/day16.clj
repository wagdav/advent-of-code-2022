(ns aoc2022.day16
  (:require [aoc2022.search :as search]
            [clojure.string :as str]))

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
  (->> (str/split-lines input)
       (mapv #(re-seq #"\d+|[A-Z][A-Z]" %))
       (mapv (fn [[from rate & to]] [from [(Integer/parseInt rate) (set to)]]))
       (into {})))

(def caves (parse-input example))

(defn solve-part1 [caves]
  (search/uniform-cost
    (reify search/Problem
      (actions [_ {:keys [position] :as state}]
        (caves position))
      (goal? [_ {:keys [remaining position]}]
        (<= remaining 0))
      (initial-state [_]
        {:position "AA"
         :remaining 30
         :pressure 0
         :open? #{}})
      (result [_ {:keys [position remaining pressure] :as state} action]
        (open-valve caves state action))
      (step-cost [this state action]
        (- (:pressure state)
           (:pressure (search/result this state action)))))))

(defn open-valve [caves {:keys [position remaining pressure] :as state} valve]
  (let [rate (first (caves valve))
        time-spent (inc (if (zero? rate) 0 1))
        flow (* rate (- remaining time-spent))]
    (-> state
      (update :open? conj valve)
      (assoc :position valve)
      (update :pressure #(+ % flow))
      (update :remaining #(- % time-spent)))))

(open-valve caves {:position "AA" :remaining 30 :open? #{} :pressure 0} "BB")
(open-valve caves {:position "BB" :remaining 28 :open? #{"BB"} :pressure 364} "CC")
(solve-part1 caves)
(defn solve-part2 [input])
