(ns advent2018.day1
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as string]))

; part1

(defn parse-freqs [str-freqs]
  (map edn/read-string str-freqs))

(def input
  (->
    (slurp "input/day1.txt")
    (string/split-lines)
    (parse-freqs)))

(defn calculate-freq [freqs]
  (reduce + 0 freqs))

(def output
  (calculate-freq input))

; part 2

(defn find-twice [input]
  (loop [freqs input, cf 0, seen #{0}]
    (if-not (empty? freqs)
      (let [f (first freqs), ncf (+ cf f)]
        (if (contains? seen ncf)
          ncf
          (recur (rest freqs) ncf (conj seen ncf))))
      (recur input cf seen))))

(def output2
  (find-twice input))