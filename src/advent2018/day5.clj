(ns advent2018.day5)

(def input
  (seq (slurp "input/day5.txt")))

(def lower #(Character/toLowerCase %))
(def upper #(Character/toUpperCase %))

(defn step [chars]
  (loop [[c1 & [c2 & after-c2 :as after-c1]] chars, output []]
    (cond (not c1) output
          (not c2) (conj output c1)

          (and (not= c1 c2) (= (upper c1) (upper c2)))
          (recur after-c2 output)

          :else (recur after-c1 (conj output c1)))))

(defn reacting [all-units]
  (loop [units all-units]
    (let [reduced-units (step units)]
      (if (= reduced-units units)
        reduced-units
        (recur reduced-units)))))

(comment
  (println
    "part 1"
    (count (reacting input)))

  ; Alternative using RegExp??
  (re-seq #"((?=[A-Z])\1|(?=[a-z])\1)" (str (take 20 input)))
  (re-seq #"([A-Z][A-Z]|(?=[a-z])\1)" (str (take 20 input))))

(defn remove-then-react [units]
  (let [character-set (distinct (map upper units))]
    (->>
      (pmap #(count (reacting (remove #{% (lower %)} units))) character-set)
      (apply min))))

(comment
  (println
    "part 2:"
    (remove-then-react input)))
