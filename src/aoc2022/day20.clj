(ns aoc2022.day20)

(defn parse-input [input]
   (mapv #(Integer/parseInt %)) (re-seq #"-?\d+" input))

(defn move
  "Move the i-th element in the sequence by n"
  [s i n]
  (let [remaining (dec (count s))]
    (conj
      (->> (cycle s)
           (drop i)
           (take (count s))
           (rest)
           (cycle)
           (drop (mod n remaining))
           (take remaining)
           (vec))
      (nth s i))))

(defn mix
 ([input]
  (mix input (range (count input))))
 ([input p]
  (reduce
    (fn [res n]
      (let [idx (.indexOf res n)
            amount (input n)]
        (move res idx amount)))
    (vec p)
    (range (count input)))))

(defn permutation [input p]
  (vec (for [i p] (get input i))))

(defn grove-coordinates [input p]
  (let [m (permutation input p)]
    (apply +
      (for [i [1000 2000 3000]]
        (nth (drop-while #(not= 0 %) (cycle m))
             i)))))

(defn solve-part1 [input]
  (grove-coordinates input (mix input)))

(defn solve-part2 [input]
  (let [decryption-key 811589153
        real-input (mapv #(* decryption-key %) input)
        p (nth (iterate (partial mix real-input)
                        (range (count input)))
               10)]
    (grove-coordinates real-input p)))
