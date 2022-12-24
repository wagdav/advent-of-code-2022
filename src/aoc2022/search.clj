(ns aoc2022.search
  (:require [clojure.data.priority-map :refer [priority-map-keyfn]]))

(defprotocol Problem
  (actions [this state])
  (goal? [this state])
  (initial-state [this])
  (result [this state action])
  (step-cost [this state action]))

; Russel&Norvig AI (Section 3.1.1 p 78.)
(defrecord Node [state actions path path-cost])

(defn- child-node [problem parent action]
  (let [state (result problem (:state parent) action)]
    (map->Node
      {:state state
       :actions (conj (:actions parent) action)
       :path (conj (:path parent) state)
       :path-cost (+ (:path-cost parent)
                     (step-cost problem (:state parent) action))})))

(defn uniform-cost [problem]
  (let [start (initial-state problem)]
    (loop [explored #{}
           frontier (priority-map-keyfn :path-cost start
                      (map->Node {:state start :actions [] :path [start]
                                  :path-cost 0}))]
      (let [[state node] (peek frontier)]
        (cond
          (empty? frontier) ; no solution
          nil

          (goal? problem state)
          node

          :else
          (recur
            (conj explored state)
            (reduce
               (fn [queue n]
                 (update queue (:state n) (fnil #(min-key :path-cost n %) n)))
               (pop frontier)
               (->> (actions problem state)
                    (map (partial child-node problem node))
                    (remove #(explored (:state %)))))))))))

(defn breadth-first [problem]
  (let [start (initial-state problem)]
    (loop [explored #{}
           frontier (conj (clojure.lang.PersistentQueue/EMPTY)
                      (map->Node {:state start :actions [] :path [start] :path-cost 0}))]
      (let [node (peek frontier)]
        (cond
          (empty? frontier) ; no solution
          nil

          (goal? problem (:state node))
          (dissoc node :state)

          :else
          (recur
            (conj explored (:state node))
            (reduce conj (pop frontier) (->> (actions problem (:state node))
                                             (map (partial child-node problem node))
                                             (remove #(explored (:state %)))))))))))
