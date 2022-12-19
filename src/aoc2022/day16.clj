(ns aoc2022.day16
  (:require [clojure.string :as str]
            [aoc2022.search :as search]))

(set! *warn-on-reflection* true)

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

(defn player-actions [{:keys [caves open?] :as state} pos]
  (for [a [:walk :open-valve]
        c (into [pos] (second (caves pos)))
        :when (or
                (and (= a :open-valve)
                     (= pos c)                 ; in the valve's cave
                     (pos? (rate-of caves c))  ; makes sense to open
                     (nil? (open? c)))         ; not open yet
                (and (= a :walk)
                     (not= c pos)))]
    [a c]))

(defn actions [{:keys [positions] :as state}]
  (if (= (count positions) 1)
    (for [p (player-actions state (first positions))]
      [p])
    (for [[a1 c1 :as p1] (player-actions state (first positions))
          [a2 c2 :as p2] (player-actions state (second positions))
          :when (if-not (= a1 a2 :open-valve)    ; don't touch the same valve
                  (not= c1 c2)
                  true)]
      [p1 p2])))

(defn move [state who [todo valve]]
  (case todo
    :walk       (assoc-in state [:positions who] valve)
    :open-valve (update state :open? conj valve)))

(defn result [{:keys [positions] :as state} action]
  (if (= 1 (count positions))
    (-> state
        release-pressure
        (move 0 (first action))
        (update :remaining dec))
    (-> state
        release-pressure
        (move 0 (first action))
        (move 1 (second action))
        (update :remaining dec)
        (update :positions #(vec (sort %))))))

(defn goal? [{:keys [remaining]}]
  (>= 0 remaining))

(defn projected-pressure [{:keys [remaining pressure caves]}]
  (let [max-rate (->> (vals caves) (map first) (apply +))]
    (+ pressure
       (* remaining max-rate))))

(defn utility [{:keys [pressure]}]
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

        :else
        (recur (conj visited state)
               (into (pop stack) (->> (for [a (actions state)] (result state a))
                                      (remove visited)))
               best)))))

(defn search-a* [start]
  (search/uniform-cost
    (reify search/Problem
      (initial-state [_]
        start)
      (goal? [_ state]
        (= 0 (:remaining state)))
      (actions [_ state]
        (actions state))
      (result [_ state action]
        (result state action))
      (step-cost [_ state action]
        (let [{:keys [caves open?] :as new-state} (result state action)
              unopened-rates (reduce + (for [c (keys caves) :when (not (open? c))] (rate-of caves c)))]
          (reduce +
            (map (fn [[todo valve]]
                   (case todo
                     :walk       unopened-rates
                     :open-valve (- unopened-rates (rate-of caves valve))))
                 action)))))))

;(let [{:keys [caves open?] :as new-state} (result state action)
;      unopened-rates (for [c (keys caves) :when (not (open? c))]
;                        (rate-of caves c))))
;  (if (seq unopened-rates)
;    (dec (apply max unopened-rates))
;    0))

(defn initial-state [caves]
  {:caves caves
   :positions ["AA"]
   :remaining 30
   :pressure 0
   :open? #{}})

(defn solve-part1 [caves]
  (:pressure (search (initial-state caves))))

(defn solve-part1* [caves]
  (:pressure (:state (search-a* (initial-state caves)))))

(defn solve-part2 [caves]
  (:pressure (search (-> (initial-state caves)
                         (assoc :remaining 26)
                         (update :positions conj "AA")))))

(defn solve-part2* [caves]
  (:pressure (:state (search-a* (-> (initial-state caves)
                                    (assoc :remaining 26)
                                    (update :positions conj "AA"))))))

(comment
  (require '[taoensso.tufte :as tufte :refer (defnp p profiled profile)])
  (tufte/add-basic-println-handler! {})

  (result (initial-state caves) [[:walk "BB"]])
  (actions (assoc (initial-state caves) :positions ["BB"]))
  (time (solve-part1 caves)) ; should be 1651
  (time (solve-part2 caves)) ; should be 1707

  (time (solve-part1* caves)) ; should be 1651
  (time (solve-part2* caves)) ; should be 1707
  (profile {}
    (solve-part1* (parse-input (slurp (clojure.java.io/resource "day16.txt")))))
  (time (solve-part2* (parse-input (slurp (clojure.java.io/resource "day16.txt")))))

  (time (search-a* (initial-state caves)))

  (profile
    {}
    (dotimes [_ 1000]
      (p :solve (solve-part2* caves)))))
