;; # Advent of Code 2022, Day 16

(ns aoc22.day15
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.set :as set]
            [nextjournal.clerk :as clerk]))

;; ## Input parsing

;; Parsing the input file is not super complicated. We use the same
;; general model for parsing AoC input. Write a `parse-file` function
;; to parse input stored on disk, a `parse-string` function for
;; creating test data on the fly in a string, and finally a
;; `parse-line` function that does all the work.

;; The only slightly tricky bit is that the plurals of the words
;; changes according to how many valves are involved.

(def input-re #"Valve ([A-Z][A-Z]) has flow rate=(\d+); tunnels? leads? to valves? (.*)")

(defn parse-line [line]
  (let [matches (re-matches input-re line)]
    {:source (keyword (nth matches 1))
     :flow (parse-long (nth matches 2))
     :dest (map keyword (str/split (nth matches 3) #", "))}))

(defn parse-string [s]
  (map parse-line (str/split-lines s)))

(defn parse-file [file]
  (map parse-line
       (-> (slurp (io/resource (str "aoc22/" file))) str/split-lines)))

;; The input is returned as a sequence of maps with valve names
;; converted to keywords and the flow rate converted to an integer:

(parse-file "day16-sample.txt")

;; ## Working

(comment

  ;; Clerk, do your thing!

  (clerk/serve! {:watch-paths ["src"] :browse? true})
  (clerk/show! "src/aoc22/day16.clj")

  :end)

;; ## Answers

(defn part-1
  "Run with bb -x aoc22.day16/part-1"
  [_]
  (prn nil))

(defn part-2
  "Run with bb -x aoc22.day16/part-2"
  [_]
  (prn nil))
