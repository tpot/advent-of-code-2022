(ns aoc22.day03
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.set :as set]
            #?(:cljs [promesa.core :as p])))

;; Read and parse input
#?(:clj
   (def input (->> (slurp (io/resource "aoc22/day03.txt"))
                   str/split-lines))
   :cljs
   (def input (await (p/->> (slurp "resources/aoc22/day03.txt")
                            str/split-lines))))

(defn pri
  "Return priority of an item a-zis> 1-26, A-Z is 27-52"
  [ch]
  (let [c (int ch)
        l (- c (int \a))
        u (- c (int \A))
        case (if (< l 0) :upper :lower)]
    (if (= case :lower) (+ l 1) (+ u 27))))

(comment

  (pri \a) ; => 1
  (pri \A) ; => 27

  (->> ["vJrwpWtwJgWrhcsFMMfFFhFp"]
       (map #(split-at (/ (count %) 2) %))
       (map #(hash-map :first (map pri (first %)) :second (map pri (second %))))) ; => ({:second (8 3 19 32 39 39 6 32 32 8 32 16), :first (22 36 18 23 16 49 20 23 36 7 49 18)})

  (let [rs {:first [22 36 18 23 16 49 20 23 36 7 49 18]
            :second [8 3 19 32 39 39 6 32 32 8 32 16]}]
    (apply set/intersection (map set (vals rs)))) ; => #{16}

  (->> ["vJrwpWtwJgWrhcsFMMfFFhFp" "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL" "PmmdzqPrVvPwwTWBwg"]
       (map #(map pri %))
       (map set)
       (apply set/intersection)
       first) ; => 18

  (->> [["vJrwpWtwJgWrhcsFMMfFFhFp" "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL" "PmmdzqPrVvPwwTWBwg"]
        ["wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn" "ttgJtRGJQctTZtZT" "CrZsJsPPZsGzwwsLwLmpwMDw"]]
       (map (fn [g] (->> g
                         (map #(map pri %))
                         (map set)
                         (apply set/intersection)
                         first)))) ; => (18 52)

  :end)

;; Generate results
(defn part-1
  "Run with (n)bb -x aoc22.day03/part-1"
  [_]
  (prn
   (->> input
        (map #(split-at (/ (count %) 2) %))
        (map #(hash-map :first (map pri (first %)) :second (map pri (second %))))
        (map #(apply set/intersection (map set (vals %))))
        (reduce concat)
        (reduce +))))

(defn part-2
  "Run with (n)bb -x aoc22.day03/part-2"
  [_]
  (prn
   (->> (partition-all 3 input)
        (map (fn [g] (->> g
                          (map #(map pri %))
                          (map set)
                          (apply set/intersection)
                          first)))
        (reduce +))))

;; TODO - generates different output in bb vs nbb
