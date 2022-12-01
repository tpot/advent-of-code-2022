(ns aoc-01
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as str]))

(def input (str/split-lines (slurp "input")))

(defn strlist->intlist [xs]
  (map #(Integer/parseInt %) xs))

(def calories-per-elf
  (->> input
       (partition-by #(= (count %) 0))
       (remove #(= % [""]))
       (map strlist->intlist)
       (map #(reduce + %))))

(comment

  (apply max calories-per-elf) ; => 66186
  (reduce + (take 3 (sort > calories-per-elf))) ; => 196804

:end)
