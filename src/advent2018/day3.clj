(ns advent2018.day3
  (:require [clojure.string :refer [split-lines]]
            [clojure.set :refer [difference index]]
            [clojure.data :as data]))

; part 1
(def line-format #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")

(defn parse-line [line]
  (let [[_ & nums] (re-matches line-format line)
        [id start-x start-y size-x size-y] (map #(Integer/parseInt %) nums)]
    {:id      id
     :start-x start-x :start-y start-y
     :size-x  size-x :size-y size-y
     :points  (for [x (range start-x (+ start-x size-x))
                    y (range start-y (+ start-y size-y))]
                {:x x :y y})}))

(def input
  (->> (slurp "input/day3.txt")
       (split-lines)
       (map parse-line)))


(defn count-duplicates [claims]
  (let [points (mapcat :points claims)
        grouped (group-by identity points)
        duplicates (map key (filter (comp #(> % 1) count val) grouped))]
    (count duplicates)))

#_
(println
  "part 1"
  (count-duplicates input))

; part 2

(defn has-overlapse? [c-claim claims]
  (let [points (set (mapcat :points (remove #{c-claim} claims)))]
    (some points (:points c-claim))))

(defn find-not-overlapping-id [claims]
  (->>
    (filter (complement #(has-overlapse? % claims)) claims)
    (first)
    :id))

#_(println
    "part 2"
    (find-not-overlapping-id input))

