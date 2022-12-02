(ns aoc22.day01
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.string :as str]
            #?(:cljs [promesa.core :as p])))

#?(:clj
   (def input (str/split-lines (slurp (io/resource "aoc22/day01.txt"))))
   :cljs
   (def input (await (p/->> (slurp "resources/aoc22/day01.txt") str/split-lines))))

(defn strlist->intlist [xs]
  #?(:clj
     (map #(Integer/parseInt %) xs)
     :cljs
     (map parse-long xs)))

(def calories-per-elf
  (->> input
       (partition-by #(= (count %) 0))
       (remove #(= % [""]))
       (map strlist->intlist)
       (map #(reduce + %))))

(defn part-1
  "Run with (n)bb -x aoc22.day01/part-1"
  [_]
  (prn
   (apply max calories-per-elf)))

(defn part-2
  "Run with (n)bb -x aoc22.day02/part-2"
  [_]
  (prn
   (reduce + (take 3 (sort > calories-per-elf)))))
