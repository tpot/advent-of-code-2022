(ns aoc22.day04
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (-> (slurp (io/resource "aoc22/day04.txt")) str/split-lines))

(defn range->set [r]
  (let [[start end] (map parse-long (str/split r #"-"))]
    (set (range start (+ end 1)))))

(defn parse-line [line]
  (map range->set (-> line (str/split #","))))

(comment

  (range->set "1-4") ; => #{1 4 3 2}
  (parse-line "2-4,6-8") ; => (#{4 3 2} #{7 6 8})

  (def sample-lines ["2-4,6-8"
                     "2-3,4-5"
                     "5-7,7-9"
                     "2-8,3-7"
                     "6-6,4-6"
                     "2-6,4-8"])

  (->> sample-lines
       (map parse-line)
       (filter (fn [[a b]] (or (set/subset? a b) (set/subset? b a))))
       count) ; => 2

  (->> sample-lines
       (map parse-line)
       (filter #(> (count (apply set/intersection %)) 0))
       count) ; => 4

  :end)

(defn part-1
  "Run with bb -x aoc22.day04/part-1"
  [_]
  (prn
   (->> input
        (map parse-line)
        (filter (fn [[a b]] (or (set/subset? a b) (set/subset? b a))))
        count)))

(defn part-2
  "Run with bb -x aoc22.day04/part-2"
  [_]
  (prn
   (->> input
        (map parse-line)
        (filter #(> (count (apply set/intersection %)) 0))
        count)))
