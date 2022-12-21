(ns aoc2022.day21
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (let [[n c] (str/split line #": ")
        [a op b] (str/split c #" ")]
    (if op
      [n {:op ({"+" + "-" - "/" quot "*" *} op) :args [a b]}]
      [n (Integer/parseInt a)])))

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv parse-line)
       (into {})))

(defn evaluate [nodes start]
  (loop [visited {} stack [start]]
    (if-some [n (peek stack)]
      (let [{:keys [op args]} (nodes n)]
        (cond
          (and op (every? visited args))
          (recur (assoc visited n (apply op (for [arg args] (visited arg))))
                 (pop stack))

          op
          (recur visited (into stack args))

          :else
          (recur (assoc visited n (nodes n))
                 (pop stack))))
      ; stack is empty
      (visited start))))

(defn solve-part1 [input]
  (evaluate input "root"))

(defn fix-root
  "For part 2, use `compare` at the root node and arrange the arguments such
  that the left subtree is smaller than the right subtree."
  [input]
  {:post [#(pos? (evaluate % "root"))]}
  (let [with-compare (assoc-in input ["root" :op] compare)]
    (if (neg? (evaluate with-compare "root"))
      (update-in with-compare ["root" :args] reverse)
      with-compare)))

(defn yell [input h]
  (assoc input "humn" h))

(defn bounds [input]
  (let [upper (loop [h (input "humn")]
                (if (pos? (evaluate (yell input h) "root"))
                  (recur (* 10 h))
                  h))
        lower (quot upper 10)]
    [lower upper]))

(defn bisect [f [lo hi]]
  {:pre [(< lo hi) (= -1 (* (f lo) (f hi)))]}
  (loop [lo lo hi hi]
    (let [c (quot (+ lo hi) 2)]
      (case (* (f lo) (f c))
         0  c
         1  (recur c hi)
        -1  (recur lo c)))))

(defn solve-part2 [input]
  (let [fixed (fix-root input)]
    (bisect #(evaluate (yell fixed %) "root")
            (bounds fixed))))
