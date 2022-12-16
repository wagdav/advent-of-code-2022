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
      (actions [_ {:keys [open? position] :as state}]
        (let [cs (second (caves position))]
          (concat
            (for [c cs :when (and (pos? (rate-of caves c)) (not (open? c)))] [:open-valve c])
            (for [c cs] [:walk c]))))
      (goal? [_ {:keys [remaining position]}]
        (>= 0 remaining))
      (initial-state [_]
        {:position "AA"
         :remaining 30
         :pressure 0
         :open? #{}})
      (result [_ {:keys [position remaining pressure] :as state} action]
        (open-valve caves state action))
      (step-cost [this state action]
        (let [{:keys [remaining open?]} (search/result this state action)]
          (* -1 remaining (apply + (for [v open?] (rate-of caves v)))))))))

(defn rate-of [caves valve]
  (first (caves valve)))

(defn release-pressure [{:keys [open?] :as state} caves]
  (update state :pressure #(apply +  % (for [c open?] (rate-of caves c)))))

(defn open-valve [caves {:keys [position open? remaining pressure] :as state} [todo valve]]
  (case todo
    :walk
    (-> state
      (release-pressure caves)
      (assoc :position valve)
      (update :remaining dec))

    :open-valve
    (-> state
      (release-pressure caves)
      (update :open? conj valve)
      (update :remaining dec))))

(open-valve caves {:position "AA" :remaining 30 :open? #{} :pressure 0} [:open-valve "BB"])
(open-valve caves {:position "AA" :remaining 30 :open? #{} :pressure 0} [:walk "BB"])
(open-valve caves {:position "BB" :remaining 28 :open? #{"BB"} :pressure 0} [:open-valve "CC"])
(solve-part1 caves) ; should be 1651
(defn solve-part2 [input])
