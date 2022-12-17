(ns aoc2022.day16
  (:require [clojure.string :as str]))

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

(defn rate-of [caves valve]
  (first (caves valve)))

(defn total-rate [{:keys [caves open?]}]
  (apply + (for [c open?] (rate-of caves c))))

(defn release-pressure [state]
  (update state :pressure #(+ % (total-rate state))))

(defn actions [{:keys [caves open? me]}]
  (let [cs (second (caves me))]
    (reverse
      (cond-> (for [c cs :when (not= c me)] [:walk c])
              (and (pos? (rate-of caves me)) (nil? (open? me)))
              (conj [:open-valve me])))))

(defn result [state [todo valve]]
  (case todo
    :walk
    (-> state
      (release-pressure)
      (assoc :me valve)
      (update :remaining dec))

    :open-valve
    (-> state
      (release-pressure)
      (update :open? conj valve)
      (update :remaining dec))))

(defn goal? [{:keys [remaining]}]
  (>= 0 remaining))

(defn projected-pressure [{:keys [remaining pressure caves] :as state}]
  (let [max-rate (->> (vals caves) (map first) (apply +))]
    (+ pressure
       (* remaining max-rate))))

(defn utility [{:keys [remaining pressure caves] :as state}]
  pressure)

(defn worse-than? [best state]
  (< (projected-pressure state) (utility best)))

(defn search [state]
  (loop [visited #{}
         stack [state]
         best state]
    (let [state (peek stack)]
      (cond
        (empty? stack)
        best

        (or (goal? state)
            (worse-than? best state))
        (recur (conj visited state)
               (pop stack)
               (max-key utility best state))

        :otherwise
        (recur (conj visited state)
               (into (pop stack) (->> (for [a (actions state)] (result state a))
                                      (remove visited)))
               best)))))

(defn initial-state [caves]
  {:caves caves
   :me "AA"
   :remaining 30
   :pressure 0
   :open? #{}})

(defn solve-part1 [caves]
  (:pressure (search (initial-state caves))))

(comment
  (result (initial-state caves) [:walk "BB"])
  (utility (initial-state caves))
  (time (solve-part1 caves))) ; should be 1651
(defn solve-part2 [input])
