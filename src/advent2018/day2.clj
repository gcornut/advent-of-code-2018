(ns advent2018.day2
  (:require [clojure.string :as string]))


(def input
  (-> (slurp "input/day2.txt")
      (string/split-lines)))

; part one

(defn calc-checksum [ids]
  (->>
    ids
    (map
      (comp
        (juxt
          #(some (comp #{2} val) %)
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

