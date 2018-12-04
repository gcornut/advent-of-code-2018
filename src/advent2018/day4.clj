(ns advent2018.day4
  (:require [clojure.string :refer [split-lines starts-with?]]
            [clojure.instant :as inst]))

; part 1

(def line-format #"\[(\d+-\d+-\d+) (\d+:(\d+))\] (.*)")

(defn parse-line [line]
  (let [wakes-up?     #{"wakes up"}
        falls-asleep? #{"falls asleep"}
        guard-shift?  #(starts-with? % "Guard")
        [_ date time minute event] (re-matches line-format line)]
    {:instant       (inst/read-instant-date (str date "T" time))
     :minute        (Integer/parseInt minute)
     :wakes-up?     (wakes-up? event)
     :falls-asleep? (falls-asleep? event)
     :guard-shift?  (guard-shift? event)
     :event         event}))

(def input
  (->> (slurp "input/day4.txt")
       split-lines
       (map parse-line)
       (sort-by :instant)))

(defn parse-guard-id [{event :event}]
  (let [[_ id] (re-matches #".*#(\d+).*" event)]
    (when id (Integer/parseInt id))))


(defn add-range-minutes [minutes id start length]
  (concat minutes
          (for [minute (range start (inc length))]
            {:id id :minute minute})))

(defn list-minutes-asleep [all-events]
  (loop [minutes []
         [event & events] all-events
         id nil
         start 0
         length 0]
    (if-not event
      (add-range-minutes minutes id start 59)
      (cond (:guard-shift? event)
            (recur minutes events (parse-guard-id event) start length)

            (:wakes-up? event)
            (recur
              (add-range-minutes minutes id start (dec (:minute event)))
              events id 0 0)

            (:falls-asleep? event)
            (recur minutes events id (:minute event) 0)))))


(defn biggest-group [groups]
  (apply max-key (comp count val) groups))

(defn result1 [minutes-asleep]
  (let [[id most-asleep-guard] (biggest-group (group-by :id minutes-asleep))
        [minute _] (biggest-group (group-by :minute most-asleep-guard))]
    (* minute id)))

(comment
  (println
    "part 1"
    (result1 (list-minutes-asleep input))))


; part two

(defn result2 [minutes-asleep]
  (let [by-guard (map val (group-by :id minutes-asleep))
        most-asleep-minute-by-guard (map #(biggest-group (group-by :minute %)) by-guard)
        [{id :id minute :minute}] (val (biggest-group most-asleep-minute-by-guard))]
    (* minute id)))

(comment
  (println
    "part 2"
    (result2 (list-minutes-asleep input))))
