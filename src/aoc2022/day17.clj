(ns aoc2022.day17
  (:require [clojure.string :as str]
            [clojure.set :as s]))

(defn parse-input [input]
  (map {\< [-1 0] \> [1 0]} (str/trim input)))

(def WIDTH 7)

(def shapes [[[0 0] [1 0] [2 0] [3 0]]
             [[1 0] [0 1] [1 1] [2 1] [1 2]]
             [[0 0] [1 0] [2 0] [2 1] [2 2]]
             [[0 0] [0 1] [0 2] [0 3]]
             [[0 0] [0 1] [1 0] [1 1]]])

(defn height [board]
  (if (empty? board)
    0
    (->> board (map second) (apply max))))

(defn collision? [board shape]
  (or
    (not (every? (fn [[x _]] (<= 0 x (dec WIDTH))) shape))
    (seq (s/intersection board (set shape)))))

(defn move [shape delta]
  (mapv #(mapv + delta %) shape))

(defn apply-wind [board shape delta]
  (let [moved (move shape delta)]
    (if (collision? board moved)
       shape
       moved)))

(defn fall [shape]
  (move shape [0 -1]))

(defn draw [board shape pos]
  (into board (move shape pos)))

(def floor (set (for [x (range 0 WIDTH)] [x 0])))

(defn appear [board shape]
  (move shape [2 (+ 4 (height board))]))

(defn game [input end]
  (loop [shapes (cycle shapes)
         winds (cycle input)
         board floor
         shape nil
         rocks 0]
    (if (= end rocks)
      board
      (let [shape (or shape (appear board (first shapes)))
            moved (apply-wind board shape (first winds))
            fallen (fall moved)]
        (if (collision? board fallen)
          (recur (rest shapes) (rest winds) (into board moved) nil (inc rocks))
          (recur shapes (rest winds) board fallen rocks))))))

(defn top-rows [board]
  (let [max-y (->> board (map second) (apply max))]
    (set
      (for [[x y] board :when (<= (- max-y y) 30)]
        [x (- max-y y)]))))

(defn game2 [input end]
  (loop [shapes (cycle shapes)
         winds (cycle input)
         board floor
         shape nil
         rocks 0
         sigs {}
         added 0]
    (if (= end rocks)
      (+ (height board) added)
      (let [wind (first winds)
            shape (or shape (appear board (first shapes)))
            moved (apply-wind board shape wind)
            fallen (fall moved)]
        (if (collision? board fallen)
          (let [b (into board moved)
                s [(first wind) (mod rocks 5) (top-rows b)]]
            (if-let [[oldt oldy] (sigs s)]
              (let [dy (- (height b) oldy)
                    dt (- rocks oldt)
                    amt (quot (- end rocks) dt)]
                (recur (rest shapes) (rest winds) (into board moved) nil
                       (+ (inc rocks) (* amt dt))
                       sigs
                       (+ added (* amt dy))))
              (recur (rest shapes) (rest winds) (into board moved) nil (inc rocks)
                     (assoc sigs s [rocks (height b)])
                     added)))
          (recur shapes (rest winds) board fallen rocks sigs added))))))

(defn viz [board]
  (doseq [row (range (height board))]
    (print row " ")
    (doseq [col (range 0 WIDTH)]
      (cond
        (board [col row]) (print "@")
        :else (print ".")))
    (println)))

(def trillion 1000000000000)

(defn solve-part1 [input]
  (height (game input 2022)))

(defn solve-part2 [input]
  (game2 input trillion))
