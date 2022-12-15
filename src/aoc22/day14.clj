(ns aoc22.day14
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.test :as t]))

;;; Input parsing

(defn parse-line [s]
  (->> (-> s (str/split #" -> "))
       (mapcat #(str/split % #","))
       (map parse-long)
       (partition 2)))

(defn parse-string [s]
  (->> (str/split-lines s) (map parse-line)))

(defn parse-file [file]
  (-> (slurp (io/resource (str "aoc22/" file))) parse-string))

(t/deftest parse-string-test
  (t/testing "String parsing"
    (t/is
     (= [[[498 4] [498 6] [496 6]] [[503 4] [502 4] [502 9] [494 9]]]
        (parse-string "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9")
        ))))

;;; Helpers

(comment

  :end)

;;; Answers!

(defn part-1
  "Run with bb -x aoc22.day14/part-1"
  [_]
  (prn nil))

(defn part-2
  "Run with bb -x aoc22.day14/part-2"
  [_]
  (prn nil))

(t/run-tests)
