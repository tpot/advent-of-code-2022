(ns aoc22.day05
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (-> (slurp (io/resource "aoc22/day05.txt")) str/split-lines))

(def sample-input ["    [D]    "
                   "[N] [C]    "
                   "[Z] [M] [P]"
                   " 1   2   3 "
                   ""
                   "move 1 from 2 to 1"
                   "move 3 from 1 to 3"
                   "move 2 from 2 to 1"
                   "move 1 from 1 to 2"])

(defn parse-crate-line
  "Parse line from crate stack diagram"
  [line]
  (let [cols (map str/trim (re-seq #"... ?" line))]
    (map #(if (str/blank? %) nil (keyword (subs % 1 2))) cols)))

(defn parse-procedure-line
  "Parse line from procedure for stacking crates"
  [line]
  (let [matches (re-matches #"move (\d+) from (\d+) to (\d+)" line)
        result {:num-crates (nth matches 1)
                :from-stack (nth matches 2)
                :to-stack (nth matches 3)}]
    (zipmap (keys result) (map parse-long (vals result)))))

(defn row->col [m]
  (map (fn [col] (filter some? (map #(nth % col) m)))
       (range (count (first m)))))

(defn parse-input [input]
  (let [[crates proc] (->> input
                           (remove str/blank?)
                           (remove #(re-matches #"[0-9 ]+" %))
                           (split-with #(not (str/starts-with? % "move"))))]
    {:crates (row->col (map parse-crate-line crates))
     :procedure (map parse-procedure-line proc)}))

(defn next-state-9000 [state move]
  (let [{:keys [num-crates from-stack to-stack]} move
        from-ndx (- from-stack 1)
        to-ndx (- to-stack 1)
        [crates-taken crates-left] (split-at num-crates (nth state from-ndx))
        crates-dest (nth state to-ndx)]
    (-> (into [] state)
        (assoc from-ndx crates-left)
        (assoc to-ndx (concat (reverse crates-taken) crates-dest)))))

(defn next-state-9001 [state move]
  (let [{:keys [num-crates from-stack to-stack]} move
        from-ndx (- from-stack 1)
        to-ndx (- to-stack 1)
        [crates-taken crates-left] (split-at num-crates (nth state from-ndx))
        crates-dest (nth state to-ndx)]
    (-> (into [] state)
        (assoc from-ndx crates-left)
        (assoc to-ndx (concat (reverse crates-taken) crates-dest)))))

(comment

  (parse-crate-line "    [D]    ") ; => (nil :D nil)
  (row->col [[nil :D nil] [:N :C nil] [:Z :M :P]]) ; => ((:N :Z) (:D :C :M) (:P))

  (parse-procedure-line "move 1 from 2 to 3") ; => {:num-crates 1, :from-stack 2, :to-stack 3}

  (parse-input sample-input)
  ;; => {:crates ((:N :Z) (:D :C :M) (:P)),
  ;;     :procedure ({:num-crates 1, :from-stack 2, :to-stack 1}
  ;;                 {:num-crates 3, :from-stack 1, :to-stack 3}
  ;;                 {:num-crates 2, :from-stack 2, :to-stack 1}
  ;;                 {:num-crates 1, :from-stack 1, :to-stack 2})}


  (next-state [[:N :Z] [:D :C :M] [:P]] {:num-crates 1 :from-stack 2 :to-stack 1})
  ;; => [(:D :N :Z) (:C :M) [:P]]

  (let [{:keys [crates procedure]} (parse-input sample-input)]
    (loop [c crates
           p procedure]
      (let [nc (next-state c (first p))]
        (if-not (nil? (next p)) (recur nc (next p)) nc)))) ; => [(:C) (:M) (:Z :N :D :P)]

  :end)

(defn part-1
  "Run with bb -x aoc22.day05/part-1"
  [_]
  (println
   (->> (let [{:keys [crates procedure]} (parse-input input)]
          (loop [c crates
                 p procedure]
            (let [nc (next-state-9000 c (first p))]
              (if-not (nil? (next p)) (recur nc (next p)) nc))))
        (map first)
        (map name)
        (str/join ""))))

(defn part-1
  "Run with bb -x aoc22.day05/part-1"
  [_]
  (println
   (->> (let [{:keys [crates procedure]} (parse-input input)]
          (loop [c crates
                 p procedure]
            (let [nc (next-state-9001 c (first p))]
              (if-not (nil? (next p)) (recur nc (next p)) nc))))
        (map first)
        (map name)
        (str/join ""))))

;; Ugh - using copy and paste for the different crane types is terrible
