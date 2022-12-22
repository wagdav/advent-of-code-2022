(ns aoc2022.day22-part2
  (:require [clojure.string :as str]
            [clojure.set :refer [map-invert]]))

(defn pad [v len]
  (if (= len (count v))
    v
    (into v (repeat (- len (count v)) \space))))

(defn parse-path [line]
  (map (fn [p]
         (if (#{"L" "R"} p) p (Integer/parseInt p)))
       (re-seq #"\d+|[RL]" line)))

(defn parse-board [lines]
  (let [width (apply max (map count lines))
        height (count lines)]
    {:width width
     :height height
     :board
      (->> lines
           (mapv vec)
           (mapv #(pad % width)))}))

(defn partitionv [n s]
  (map vec (partition n s)))

(defn parse-cube [input]
  (let [tiles (count (re-seq #"\S" input))
        edge-length (int (Math/sqrt (quot tiles 6)))
        lines (str/split-lines input)
        edge-top-left-corners (->> (take-nth edge-length lines)
                                   (mapv #(take-nth edge-length %)))
        face-coordinates (zipmap
                           (for [[i r] (map-indexed vector edge-top-left-corners)
                                 [j c] (map-indexed vector r)
                                 :when (not= c \space)]
                             [i j])
                           (iterate inc 1))
        face-connections (apply merge-with conj
                           (for [[[ir ic] i] face-coordinates
                                 [[jr jc] j] face-coordinates
                                 :when (not= i j)]
                             (cond
                               ; same row - on i is right of j
                               (and (= ir jr) (= ic (inc jc))) {j {:right i} i {:left j}}
                               ; same column - i is below j
                               (and (= ic jc) (= ir (inc jr))) {j {:down i} i {:up j}}

                               :else {})))]
    {:edge-length edge-length
     :edge-top-left-corners edge-top-left-corners
     :face-coordinates face-coordinates
     :face-connections face-connections
     :faces
     (->> lines
          (map str/trim)
          (partition edge-length)
          (map (fn [line] (map #(partitionv edge-length %) line)))
          (mapcat #(apply map vector %))
          vec)}))

(defn parse-input [input]
  (let [[board path] (str/split input #"\R\R")]
    (merge
      (parse-board (str/split-lines board))
      (parse-cube board)
      {:path (parse-path path)})))

(defn pos [{:keys [board width height]} [row col]]
  (:pre [(pos? row) (pos? col)])
  (get-in board [(mod (dec row) height)
                 (mod (dec col) width)]))

(def facing {0 :right 1 :down 2 :left 3 :up})
(def directions {0 [0 1] 1 [1 0] 2 [0 -1] 3 [-1 0]})

(defn turn [state d]
  (update state :facing
    #(mod (case d
           "R" (inc %)
           "L" (dec %))
      4)))

(def opposite
  {:left :right
   :right :left
   :up :down
   :down :up})

(defn ahead
  "Return the map-coordinate of the point ahead."
  [{:keys [position facing face-connections edge-length] :as state}]
  (let [[cs cr cc] position
        [r c] (mapv + [cr cc] (directions facing))
        moving-to (cond
                    (zero? c)         :left
                    (> c edge-length) :right
                    (zero? r)         :up
                    (> r edge-length) :down)]
    (if moving-to
      (let [new-side (get-in face-connections [cs moving-to])
            from (-> (face-connections new-side) (map-invert) (get cs))
            compl (fn [x] (inc (- edge-length x)))
            [new-row new-col] (case [moving-to from]
                                ; r can be used
                                [:right :right]  [(compl r) edge-length]
                                [:right :down]   [edge-length r]
                                [:right :left]   [r 1]
                                [:right :up]     [1 (compl r)]
                                ; c can be used
                                [:down  :right]  [c edge-length]
                                [:down  :down]   [edge-length (compl c)]
                                [:down  :left]   [(compl c) 1]
                                [:down  :up]     [1 c]
                                ; r can be used
                                [:left  :right]  [r edge-length]
                                [:left  :down]   [edge-length (compl r)]
                                [:left  :left]   [(compl r) 1]
                                [:left  :up]     [1 r]
                                ; c can be used
                                [:up    :right]  [c edge-length]
                                [:up    :down]   [edge-length c]
                                [:up    :left]   [c 1]
                                [:up    :up]     [edge-length (compl c)])]
         (assoc state :position [new-side new-row new-col]
                      :facing ({:right 0 :down 1 :left 2 :up 3} (opposite from))))
      (assoc state :position [cs r c]))))

(defn at [{:keys [faces position]}]
  (get-in faces (map dec position)))

(defn advance [state steps]
  (loop [i steps s state]
    (let [next-s (ahead s)]
      (if (or (zero? i) (= (at next-s) \#))
        s
        (recur (dec i) next-s)))))

(defn initial-state [state cube]
  (-> state
      (merge {:facing 0
              :position [1 1 1] ; [side row col]
              :face-connections cube})))

(defn password [position facing]
  (let [[row col] position]
    (+ (* 1000 row)
       (* 4 col)
       facing)))

(defn cube->map [{:keys [face-coordinates edge-length position]}]
  (let [[side x y ] position
        m (map-invert face-coordinates)
        [xo yo] (m side)]
    [(+ x (* xo edge-length))
     (+ y (* yo edge-length))]))

(defn- set-current [{:keys [facing] :as state}]
  (let [[row col] (cube->map state)]
    (assoc-in state [:board (dec row) (dec col)]
                    ({0 ">" 1 "v" 2 "<" 3 "^"} facing))))

(defn viz [state]
  (let [{:keys [board]} (set-current state)]
    (doseq [row board]
      (doseq [col row]
          (print col))
      (println))
    state))

(defn solve-part2 [input cube]
  (let [end (reduce
              (fn [state m]
                 (if (number? m)
                   (advance state m)
                   (turn state m)))
              (initial-state input cube)
              (:path input))]
   (password (cube->map end) (end :facing))))

; Cubes folded manually
(def cube-example
 {2 {:left 6 :right 3 :up 1 :down 5}
  3 {:left 2 :right 4 :up 1 :down 5}
  1 {:left 3 :right 6 :up 2 :down 4}
  4 {:left 3 :right 6 :up 1 :down 5}
  5 {:left 3 :right 6 :up 4 :down 2}
  6 {:left 5 :right 1 :up 4 :down 2}})

(def cube-real
 {1 {:left 4 :right 2 :up 6 :down 3}
  2 {:left 1 :right 5 :up 6 :down 3}
  3 {:left 4 :right 2 :up 1 :down 5}
  4 {:left 1 :right 5 :up 3 :down 6}
  5 {:left 4 :right 2 :up 3 :down 6}
  6 {:left 1 :right 5 :up 4 :down 2}})
