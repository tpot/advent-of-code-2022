(ns aoc22.day06
  (:require [clojure.java.io :as io]))

(def input (slurp (io/resource "aoc22/day06.txt")))

(defn mchar [input n]
  (->> (loop [x input res []]
         (if (< (count x) n) res (recur (drop 1 x) (cons (take n x) res))))
       reverse
       (take-while #(not (apply distinct? %)))
       count
       (+ n)))

(defn part-1
  "Run with bb -x aoc22.day06/part-1"
  [_]
  (prn (mchar input 4))) ; => 1198

(defn part-2
  "Run with bb -x aoc22.day02/part-2"
  [_]
  (prn (mchar input 14))) ; => 3120
