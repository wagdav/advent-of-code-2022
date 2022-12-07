(ns aoc2022.day07
  (:require [clojure.string :as str]))

(defn command [state [cmd-name arg]]
  (case cmd-name
    "ls" state
    "cd" (case arg
          ".." (update state :cwd #(vec (butlast %)))
          "/"  (assoc state :cwd ["/"])
          (update state :cwd conj arg))))

(defn entry [{:keys [cwd] :as state} [size file-name]]
  (if (= size "dir")
    state
    (assoc-in state (conj cwd file-name) (Integer/parseInt size))))

(defn parse-line [state line]
  (let [parts (str/split line #" ")]
    (if (= "$" (first parts))
      (command state (rest parts))
      (entry state parts))))

(defn parse-input [input]
  (reduce parse-line {:cwd ["/"]} (str/split-lines input)))

(defn total-size [fs path]
  (apply +
    (for [[k s] (get-in fs path)]
      (if (map? s)
        (total-size fs (conj path k))
        s))))

(defn dirs [fs path]
  (reduce
    (fn [res [k s]]
      (if (map? s)
        (let [dir-name (conj path k)]
          (into (conj res dir-name) (dirs fs dir-name)))
        res))
    []
    (get-in fs path)))

(defn solve-part1 [input]
  (->> (dirs input ["/"])
       (map #(total-size input %))
       (filter #(<= % 100000))
       (apply +)))

(defn solve-part2 [input]
  (let [disk 70000000
        need 30000000
        total (total-size input ["/"])
        unused (- disk total)
        required (- need unused)]
     (->> (dirs input ["/"])
          (map #(total-size input %))
          sort
          (drop-while #(< % required))
          first)))
