(ns aoc2022.day16
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (let [lines (->> (str/split-lines input)
                   (map #(re-seq #"\d+|[A-Z][A-Z]" %))
                   (map (fn [[from rate & to]] [from [(Integer/parseInt rate) (set to)]])))]
    {:rates (into {} (for [[c l] lines] [c (first l)]))
     :tunnels (into {} (for [[c l] lines] [c (second l)]))}))

(defn initial-state [input]
  (merge input
   {:position "AA"
    :remaining 30
    :open? #{}
    :player 1}))

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
    (fn [{:keys [remaining position tunnels rates open? player] :as state}]
      (if (zero? remaining)
        (if (= player 1)
          0
          (total-pressure (assoc state :remaining 26
                                       :position "AA"
                                       :player (dec player))))
        (apply max
          (cond->
            (for [dest (tunnels position)]
              (total-pressure (walk state dest)))

            ; there's a valve which makes sense to open
            (and (pos? (rates position))
                 (not (open? position)))
            (conj
              (+
                (* (dec remaining) (rates position))
                (total-pressure (open-valve state position))))))))))

(defn solve-part1 [input]
  (total-pressure (initial-state input)))

(defn solve-part2 [input]
  (total-pressure (assoc (initial-state input) :remaining 26 :player 2)))
