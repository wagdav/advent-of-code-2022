(ns aoc2022.day15
  (:require [clojure.set :as s]))

(def example "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3")

(defn parse-input [input]
  (->> (re-seq #"-?\d+" input)
       (mapv #(Integer/parseInt %))
       (partition 4)
       (mapv #(partition 2 %))))

(defn manhattan [p1 p2]
  (->> (map - p1 p2)
       (map #(Math/abs ^long %))
       (apply +)))

(defn sensors [input]
  (into {} (map (fn [[s b]] [s (manhattan s b)]) input)))

(def ex (parse-input example))

(defn effect-y [y [[sx sy] r]]
  (let [d (abs (- sy y))
        e (- r d)]
    (when (<= d r)
      [(- sx e) (+ sx e)])))

(defn effect-x [x [[sx sy] r]]
  (let [d (abs (- sx x))
        e (- r d)]
    (when (<= d r)
      [(- sy e) (+ sy e)])))

(defn fix [f intervals]
  (loop [cur intervals]
    (let [new_ (f cur)]
      (if (= new_ cur)
        new_
        (recur new_)))))

(defn merge-intervals [[x1 x2] [y1 y2]]
  (if (or (< x2 y1) (< y2 x1)) nil [(min x1 y1) (max x2 y2)]))

(merge-intervals [-4 9] [7 25])

(defn merge-all [intervals]
  (set
    (reduce
      (fn [res i]
        (if-let [r (some #(merge-intervals % i) (remove #{i} res))]
          (conj (remove #{i} res) r)
          res))
      intervals
      intervals)))

(defn coverage-y [sensors y]
  (->> sensors
       (map (partial effect-y y))
       (filter some?)
       (fix merge-all)))

(defn coverage-x [sensors x]
  (->> sensors
       (map (partial effect-x x))
       (filter some?)
       (fix merge-all)))

(defn tuning-frequency [x y]
  (+ (* 4000000 x) y))

(defn solve-part1 [y input]
  (let [beacons (into #{} (map second input))
        ranges (coverage-y (sensors input) y)]
    (apply +
      (for [[start end] ranges]
        (let [beacons-in-range (count (filter (fn [[bx by]] (and (<= start bx end) (= y by)))
                                              beacons))]
          (inc (- end start beacons-in-range)))))))

(defn solve-part2 [limit input]
  (let [es (->> (sensors input)
                (map (fn [[p r]] [p (inc r)])))
        ps (into #{} (apply concat (for [i es j es
                                         :let [r (edge-intersect i j)]
                                         :when (not (empty? r))]
                                     r)))]
      (for [[x y] ps
            :let [rx (coverage-y (sensors input) y)
                  ry (coverage-x (sensors input) x)]
            :when (and (<= 0 x limit) (<= 0 y limit)
                       (< 1 (count rx)) (< 1 (count ry)))]
        [rx ry])))

(defn edge [[[x0 y0] r]]
  (for [m [+1 -1]
        p [[(+ x0 r) y0] [(- x0 r) y0]]]
   [m p]))

(defn line-intersect [[ma [xa ya]] [mb [xb yb]]]
  (when (not= ma mb)
    (let [x0 (/ (- (- yb ya)
                   (- (* mb xb) (* ma xa)))
                (- ma mb))
          y0a (+ ya (* ma (- x0 xa)))
          y0b (+ yb (* mb (- x0 xb)))]
      (assert (= y0a y0b))
      [x0 y0a])))

(defn edge-intersect [[p1 r1 :as e1] [p2 r2 :as e2]]
  (if (= e1 e2)
    '()
    (for [l1 (edge e1) l2 (edge e2)
          :let [p (line-intersect l1 l2)]
          :when (and p (= r1 (manhattan p1 p))
                       (= r2 (manhattan p2 p)))]
      p)))

(solve-part2 20 ex)
