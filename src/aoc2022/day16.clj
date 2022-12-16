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
    {:valve->rate (into {} (for [[c l] lines] [c (first l)]))
     :tunnels-from (into {} (for [[c l] lines] [c (second l)]))}))

(defn shortest-path [tunnels start end]
  (search/uniform-cost
    (reify search/Problem
      (actions [_ state]
        (tunnels state))
      (goal? [_ state]
        (= state end))
      (initial-state [_]
        start)
      (result [_ state action]
        action)
      (step-cost [_ _ _]
        1))))

(shortest-path (:tunnels-from ex) "AA" "AA")

(defn interesting-valve? [#{:keys [valve->rate]} valve]
  (pos? valve-> valve->rate))

(defn makes-sense? [{:keys [valve->rate open?] :as state} who [todo valve]]
  (let [pos (state who)]
    (or
      (and (= todo :open-valve)
           (= pos valve)               ; in the valve's cave
           (pos? (valve->rate valve))  ; makes sense to open
           (nil? (open? valve)))       ; not open yet
      (and (= todo :walk)
           (not= valve pos)))))

(defn find-next-big-valve [{:keys [tunnels-from valve->rate open?] :as state} who]
  (let [pos (state who)
        choice {:me first :elephant second}]
    (when-let [[next-valve _] (choice (reverse (sort-by second (->> valve->rate (remove #(open? (first %)))))))]
      (let [path (shortest-path tunnels-from pos next-valve)
            next-move (if-let [t (first (:actions path))]
                        [:walk t]
                        [:open-valve next-valve])]))))

(defn player-actions [{:keys [tunnels-from valve->rate open?] :as state} who]
  (let [pos (state who)
        steps (-> (for [a [:walk :open-valve] c (filter #(pos? (valve->rate %)) (keys valve->rate))]
                    [a c])
                  (conj (find-next-big-valve state who)))]
    (->> steps
      (remove nil?)
      (filter (partial makes-sense? state who)))))

(defn actions [{:keys [elephant] :as state}]
  (if-not elephant
    (for [p (player-actions state :me)]
      p)
    (for [[a1 c1 :as p1] (player-actions state :me)
          [a2 c2 :as p2] (player-actions state :elephant)
          :when (if (= a1 a2 :open-valve)    ; don't touch the same valve
                  (not= c1 c2)
                  true)]
      [p1 p2])))

(defn move [{:keys [tunnels-from] :as state} who [todo valve]]
  (let [start (state who)
        path (shortest-path tunnels-from start valve)]
    (case todo
      :walk       (assoc state who (first (:actions path)))
      :open-valve (update state :open? conj valve))))

(defn total-rate [{:keys [valve->rate open?]}]
  (apply + (for [v open?] (valve->rate v))))

(defn release-pressure [state]
  (update state :pressure #(+ % (total-rate state))))

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

(defn reachable-valves [tunnels-from start max-steps]
  (loop [visited #{}
         stack [[0 start]]]
    (let [[depth valve] (peek stack)]
      (cond
        (empty? stack)
        visited

       (= depth max-steps)
       (recur (conj visited valve)
              (pop stack))

       :else
       (recur (conj visited valve)
              (into (pop stack) (->> (tunnels-from valve)
                                     (remove visited)
                                     (map #(vector (inc depth) %)))))))))

(defn projected-pressure-simple [{:keys [tunnels-from me elephant open? remaining pressure valve->rate]}]
  (let [max-rate  (apply + (vals valve->rate))]
    (+ pressure
       (* remaining max-rate))))

(defn projected-pressure [{:keys [tunnels-from me elephant open? remaining pressure valve->rate]}]
  (let [reachable (if-not elephant
                    (reachable-valves tunnels-from me remaining)
                    (into (reachable-valves tunnels-from me remaining)
                          (reachable-valves tunnels-from elephant remaining)))
        max-rate  (apply + (map valve->rate (into open? reachable)))]
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

(defn initial-state [input]
  (merge input
    {:me "AA"
     :remaining 30
     :pressure 0
     :open? #{}}))

(defn solve-part1 [input]
  (:pressure (search (initial-state input))))

(defn solve-part2 [input]
  (:pressure (search (-> (initial-state input)
                         (assoc :remaining 26)
                         (assoc :elephant "AA")))))

(comment
   (def ex (parse-input example))

   (projected-pressure (initial-state ex))
   (result (initial-state ex) [:walk "BB"])
   (actions (assoc (initial-state ex) :me "BB"))
   (actions (assoc (initial-state ex) :me "BB" :elephant "BB"))
   (time (solve-part1 ex)) ; should be 1651
   (time (solve-part2 ex))) ; should be 1707
