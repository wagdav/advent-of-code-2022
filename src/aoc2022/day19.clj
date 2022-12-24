(ns aoc2022.day19)

(defn parse-input [input]
  (->> (re-seq #"\d+" input)
       (map #(Integer/parseInt %))
       (partition 7)
       (map (fn [[id ore-robot-ore clay-robot-ore obsidian-robot-ore obsidian-robot-clay geode-robot-ore geode-robot-obsidian]]
               [id {:ore {:ore ore-robot-ore}
                    :clay {:ore clay-robot-ore}
                    :obsidian {:ore obsidian-robot-ore
                               :clay obsidian-robot-clay}
                    :geode {:ore geode-robot-ore
                            :obsidian geode-robot-obsidian}}]))
       (into {})))

(defn initial-state [blueprint t]
  {:remaining t
   :blueprint blueprint
   :mined {:ore 0
           :clay 0
           :obsidian 0
           :geode 0}
   :robots {:ore 1}})

(defn plus [n]
  (fnil #(+ % n) 0))

(defn minus [n]
  (fnil #(- % n) 0))

(defn mine [{:keys [robots] :as state}]
  (-> state
    (update-in [:mined :ore] (plus (get robots :ore 0)))
    (update-in [:mined :clay] (plus (get robots :clay 0)))
    (update-in [:mined :obsidian] (plus (get robots :obsidian 0)))
    (update-in [:mined :geode] (plus (get robots :geode 0)))))

(defn actions [{:keys [robots blueprint mined]}]
  (let [needed (apply (partial merge-with max) (vals blueprint))]
    (cond->
      [[:wait]]

      (and (>= (mined :ore) (get-in blueprint [:ore :ore]))
           (> (needed :ore) (get robots :ore 0)))
      (conj [:build-robot :ore])

      (and (>= (mined :ore) (get-in blueprint [:clay :ore]))
           (> (needed :clay) (get robots :clay 0)))
      (conj [:build-robot :clay])

      (and (>= (mined :ore) (get-in blueprint [:obsidian :ore]))
           (>= (mined :clay) (get-in blueprint [:obsidian :clay]))
           (> (needed :obsidian) (get robots :obsidian 0)))
      (conj [:build-robot :obsidian])

      (and (>= (mined :ore) (get-in blueprint [:geode :ore]))
           (>= (mined :obsidian) (get-in blueprint [:geode :obsidian])))
      (conj [:build-robot :geode]))))

(defn result [{:keys [blueprint] :as state} [todo r]]
  (case todo
    :wait
    (-> state
        mine
        (update :remaining dec))
    :build-robot
    (-> state
        mine
        (update-in [:mined :ore] (minus (get-in blueprint [r :ore] 0)))
        (update-in [:mined :clay] (minus (get-in blueprint [r :clay] 0)))
        (update-in [:mined :obsidian] (minus (get-in blueprint [r :obsidian] 0)))
        (update-in [:robots r] (fnil inc 0))
        (update :remaining dec))))

(defn utility [state]
  (if (nil? state)
    0
    (get-in state [:mined :geode] 0)))

(defn goal? [{:keys [remaining]}]
  (zero? remaining))

(defn limit-stock [{:keys [blueprint mined remaining robots] :as state}]
  (let [needed (apply (partial merge-with max) (vals blueprint))
        max-ore (- (* remaining (needed :ore))
                   (* (get robots :ore 0) (dec remaining)))
        max-clay (- (* remaining (needed :clay))
                    (* (get robots :clay 0) (dec remaining)))
        max-obs (- (* remaining (needed :obsidian))
                   (* (get robots :obsidian 0) (dec remaining)))]
    (-> state
      (assoc-in [:mined :ore] (min (mined :ore) max-ore))
      (assoc-in [:mined :clay] (min (mined :clay) max-clay))
      (assoc-in [:mined :obsidian] (min (mined :obsidian) max-obs)))))

(defn dfs [t blueprint]
  (loop [explored #{}
         stack [(initial-state blueprint t)]
         best nil]
    #_(when (zero? (mod (count explored) 100000))
        (println "best" (utility best)))
    (if-let [state (peek stack)]
      (if (goal? state)
        (recur (conj explored state)
               (pop stack)
               (max-key utility state best))
        (recur (conj explored state)
               (into (pop stack)
                     (->> (actions state)
                          (map #(result state %))
                          (map limit-stock)
                          (remove explored)))
               best))
      best)))

(defn solve-part1 [input]
  (apply +
    (pmap
      (fn [[id blueprint]]
        (* id (get-in (dfs 24 blueprint) [:mined :geode] 0)))
      input)))

(defn solve-part2 [input]
  (->> (map input [1 2 3])
       (pmap (fn [blueprint] (get-in (dfs 32 blueprint) [:mined :geode])))
       (apply *)))

(comment
  (require '[clojure.java.io :as io]
           '[aoc2022.day19-test :refer [example]])
  (def real (slurp (io/resource "day19.txt")))
  (:mined (dfs 24 (get (parse-input example) 1))) ; 9 geodes
  (:mined (dfs 24 (get (parse-input example) 2))) ; 12 geodes
  (time (solve-part1 (parse-input example))) ; should be 33
  (time (solve-part1 (parse-input real)))
  (time (solve-part2 (parse-input real))))
