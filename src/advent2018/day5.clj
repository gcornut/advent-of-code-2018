(ns advent2018.day5
  (:require [clojure.string :as string])
  (:import (java.util.regex Pattern)))

(def input
  (slurp "input/day5.txt"))

(def lower #(Character/toLowerCase %))
(def upper #(Character/toUpperCase %))

(defn ^:deprecated step [chars]
  (loop [[c1 & [c2 & after-c2 :as after-c1]] chars, output []]
    (cond (not c1) output
          (not c2) (conj output c1)

          (and (not= c1 c2) (= (upper c1) (upper c2)))
          (recur after-c2 output)

          :else (recur after-c1 (conj output c1)))))

(def unit-types (map char (range (int \a) (inc (int \z)))))

(def polymer-pattern
  "RegExp pattern capturing components of a polymer (unit pairs and single units)"
  (let [unit-pairs (mapcat (fn [c] [(str c (upper c)) (str (upper c) c)]) unit-types)
        regexp (str "(" (string/join "|" unit-pairs) ")|(.)")
        pattern (Pattern/compile regexp)]
    pattern))

(defn step-regexp
  "A faster reaction step function using RegExp"
  [input]
  (let [polymer (apply str input)
        matches (re-seq polymer-pattern polymer)
        without-pairs (filter (comp nil? second) matches)]
    (map first without-pairs)))

(defn reacting [polymer]
  (loop [polymer polymer]
    (let [reacted-polymer (step-regexp polymer)]
      (if (= (count reacted-polymer) (count polymer))
        reacted-polymer
        (recur reacted-polymer)))))

(comment
  (println
    "part 1:"
    (count (reacting input))))

(defn remove-then-react [polymer]
  (->>
    (pmap #(count (reacting (remove #{% (upper %)} polymer)))
          unit-types)
    (apply min)))

(comment
  (println
    "part 2:"
    (remove-then-react input)))
