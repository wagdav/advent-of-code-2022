(ns aoc2022.day11
  (:require [clojure.string :as str]))

(defn parse-int [s]
  (Integer/parseInt (re-find #"\d+" s)))

(defn parse-block [b]
  (reduce
    (fn [out line]
      (let [[section content] (map str/trim (str/split line #":"))]
        (case section
          "Starting items"
          (assoc out :items (mapv #(Integer/parseInt %)
                                (re-seq #"\d+" content)))
          "Operation"
          (assoc out :op (let [[op arg] (drop 3 (str/split content #" "))
                               op-fn ({"*" * "+" +} op)]
                           (if (= arg "old")
                             (fn [old] (op-fn old old))
                             (fn [old] (op-fn old (Integer/parseInt arg))))))
          "Test"
          (assoc out :divisible-by (parse-int content))
          "If true"
          (assoc out :if-true (parse-int content))
          "If false"
          (assoc out :if-false (parse-int content))

          ; Monkey index
          (assoc out :index (parse-int section)))))
    {}
    (str/split-lines b)))

(defn parse-input [input]
  (mapv parse-block (str/split input #"\R\R")))

(defn inspect [{:keys [index op divisible-by if-true if-false]} state item]
  (let [level (quot (op item) 3)
        divisible? (zero? (rem level divisible-by))]
    (cond-> state
      true              (update-in [index :items] #(vec (drop 1 %)))
      true              (update-in [index :inspected] (fnil inc 0))
      divisible?        (update-in [if-true :items] conj level)
      (not divisible?)  (update-in [if-false :items] conj level))))

(defn turn [state i]
  (let [monkey (get state i)]
    (reduce (partial inspect monkey)
            state
            (monkey :items))))

(defn round [state]
  (reduce
    turn
    state
    (range (count state))))

(defn solve-part1 [input]
  (->> (nth (iterate round input) 20)
       (map :inspected)
       (sort >)
       (take 2)
       (apply *)))

(defn solve-part2 [input])

(comment
  (inspect {:index 0 :op inc :divisible-by 3 :if-true 0 :if-false 1}
           [{:index 0 :items [2 3 4]}]
           15)

  (->> (parse-input aoc2022.day11-test/example)
       (iterate round)
       (take (inc 20)))


  (solve-part1 (parse-input aoc2022.day11-test/example)))
