(ns advent2018.day2
  (:require [clojure.string :as string]
            [clojure.data :as data]))


(def input
  (-> (slurp "input/day2.txt")
      (string/split-lines)))

; part one

(defn calc-checksum [ids]
  (->>
    ids
    (map (comp
           (juxt #(some (comp #{2} val) %)
                 #(some (comp #{3} val) %))
           frequencies))
    (reduce (fn [acc [has-two? has-three?]]
              (-> acc
                  (update-in [0] + (if has-two? 1 0))
                  (update-in [1] + (if has-three? 1 0))))
            [0 0])
    (reduce *)))

(def output
  (calc-checksum input))

; part two

(defn has-one-difference? [coll]
  (= 1 (count (filter (complement nil?) coll))))

(defn find-similar-box
  "Given a box id, find in coll of box ids another box with only one character difference"
  [sbox boxes]
  (reduce (fn [_ box]
            (let [[in-a in-b in-both] (data/diff (seq sbox) (seq box))]
              (when (and (has-one-difference? in-a)
                         (has-one-difference? in-b))
                (reduced in-both))))
          boxes))

(defn find-similar-boxes [boxes]
  (some #(find-similar-box % boxes) boxes))

(defn common-characters [differences]
  (apply str differences))

(def output2
  (common-characters
   (find-similar-boxes input)))

