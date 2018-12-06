(ns advent2018.day6
  (:require [clojure.string :as string]
            [clojure.edn :as edn]))

(defn parse-line [id line]
  (let [[x y] (edn/read-string (str "[" line "]"))]
    {:x x :y y :id (str "P" id)}))

(def input
  (->>
    (slurp "input/day6.txt")
    (string/split-lines)
    (map-indexed parse-line)))

(def xs (set (map :x input)))
(def ys (set (map :y input)))
(def min-x (apply min xs))
(def min-y (apply min ys))
(def max-x (apply max xs))
(def max-y (apply max ys))

(defn in-center? [{x :x y :y}]
  (and (some #(or (> % x) (< % x)) xs)
       (some #(or (> % y) (< % y)) ys)))

(defn distance [{x1 :x y1 :y} {x2 :x y2 :y}]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn non-infinite-area []
  (remove nil?
    (for [x (range min-x (inc max-x))
          y (range min-y (inc max-y))
          :let [point {:x x :y y}]
          :when (in-center? point)]
      (let [distance-others (map (juxt identity #(distance point %)) input)
            [[{id1 :id} d1] [{id2 :id} d2]] (take 2 (sort-by second distance-others))]
        (cond (< d1 d2) id1
              (> d1 d2) id2)))))

(comment
  (println
    "part 1:"
    (apply max-key val (frequencies (non-infinite-area)))))


(defn points-in-central-region [min-distance]
  (remove nil?
    (for [x (range min-x (inc max-x))
          y (range min-y (inc max-y))
          :let [point {:x x :y y}]
          :when (in-center? point)]
      (let [d (map #(distance point %) input)
            s (reduce + d)]
        (when (< s min-distance) point)))))

(comment
  (println
    "part 2:"
    (count (points-in-central-region 10000))))
