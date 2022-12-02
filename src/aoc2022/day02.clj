(ns aoc2022.day02)

(defn parse-input [input]
  (->> (re-seq #"[A-Z]" input)
       (partition 2)))

(def code->move {"A" :rock "B" :paper "C" :scissors
                 "X" :rock "Y" :paper "Z" :scissors})

(def code->outcome {"X" :loose "Y" :draw "Z" :win})

(def move->score {:rock 1 :paper 2 :scissors 3})

(defn game-score [opponent you]
  (if (= you opponent)
    3 ; draw
    (case [you opponent]
      [:rock :scissors] 6
      [:scissors :paper] 6
      [:paper :rock] 6
      0)))

(defn total-score [opponent you]
  (+ (game-score opponent you) (move->score you)))

(defn move [opponent outcome]
  (case outcome
    :draw  opponent
    :win   (case opponent
             :rock      :paper
             :paper     :scissors
             :scissors  :rock)
    :loose (case opponent
             :rock     :scissors
             :paper    :rock
             :scissors :paper)))

(defn solve-part1 [input]
  (reduce (fn [score [abc xyz]]
            (let [opponent-move (code->move abc)
                  your-move (code->move xyz)]
              (+ score (total-score opponent-move your-move))))
          0
          input))

(defn solve-part2 [input]
  (reduce (fn [score [abc xyz]]
            (let [opponent-move (code->move abc)
                  outcome (code->outcome xyz)
                  your-move (move opponent-move outcome)]
              (+ score (total-score opponent-move your-move))))
          0
          input))
