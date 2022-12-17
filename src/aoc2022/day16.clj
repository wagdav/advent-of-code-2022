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

(defn player-actions [{:keys [open?] :as state} who]
  (let [pos (state who)]
    (for [a [:walk :open-valve]
          c (into [pos] (second (caves pos)))
          :when (or
                  (and (= a :open-valve)
                       (= pos c)                 ; in the valve's cave
                       (pos? (rate-of caves c))  ; makes sense to open
                       (nil? (open? c)))         ; not open yet
                  (and (= a :walk)
                       (not= c pos)))]
      [a c])))

(defn actions [{:keys [elephant] :as state}]
  (if-not elephant
    (for [p (player-actions state :me)]
      p)
    (for [[a1 c1 :as p1] (player-actions state :me)
          [a2 c2 :as p2] (player-actions state :elephant)
          :when (if-not (= a1 a2 :open-valve)    ; don't touch the same valve
                  (not= c1 c2)
                  true)]
      [p1 p2])))

(caves "BB")
(actions {:caves caves :open? #{} :me "BB" :elephant "BB"})
(player-actions {:caves caves :open? #{} :me "BB" :elephant "CC"} :elephant)

(defn move [state who [todo valve]]
  (case todo
    :walk       (assoc state who valve)
    :open-valve (update state :open? conj valve)))

(defn result [{:keys [elephant] :as state} action]
  (if-not elephant
    (-> state
        release-pressure
        (move :me action)
        (update :remaining dec))
    (-> state
        release-pressure
        (move :me (first action))
        (move :elephant (second action))
        (update :remaining dec))))

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

(defn initial-state [caves]
  {:caves caves
   :me "AA"
   :remaining 30
   :pressure 0
   :open? #{}})

(defn solve-part1 [caves]
  (:pressure (search (initial-state caves))))

(defn solve-part2 [caves]
  (:pressure (search (-> (initial-state caves)
                         (assoc :remaining 26)
                         (assoc :elephant "AA")))))

(comment
  (result (initial-state caves) [:walk "BB"])
  (actions (assoc (initial-state caves) :me "BB"))
  (time (solve-part1 caves)) ; should be 1651
  (time (solve-part2 caves))) ; should be 1707
