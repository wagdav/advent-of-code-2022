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
                           (fn [old] (op-fn old (if (= arg "old")
                                                  old
                                                  (Integer/parseInt arg))))))
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

(defn inspect [{:keys [index op divisible-by if-true if-false]} fun state item]
  (let [level (fun (op item))
        to-index (if (zero? (rem level divisible-by))
                     if-true
                     if-false)]
    (-> state
      (update-in [index :inspected] (fnil inc 0))
      (update-in [index :items] #(vec (drop 1 %)))
      (update-in [to-index :items] conj level))))

(defn turn [fun state i]
  (let [monkey (get state i)]
    (reduce (partial inspect monkey fun)
            state
            (monkey :items))))

(defn round [fun state]
  (reduce
    (partial turn fun)
    state
    (range (count state))))

(defn simulate [input num-turns fun]
  (nth (iterate (partial round fun) input) num-turns))

(defn monkey-business [state]
   (->> (map :inspected state)
        (sort >)
        (take 2)
        (apply *)))

(defn solve-part1 [input]
  (monkey-business (simulate input 20 #(quot % 3))))

(defn solve-part2 [input]
  (let [n (apply * (map :divisible-by input))]
    (monkey-business (simulate input 10000 #(mod % n)))))
