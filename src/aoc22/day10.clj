(ns aoc22.day10
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.set :as set]))

;;; Input parsing

(defn parse-line
  "Parse space-separated arguments from input line."
  [line]
  (let [[opcode & args] (str/split line #" ")]
    (into {:opcode (keyword opcode)} (and args {:arg (parse-long (first args))}))))

(defn parse-input
  "Read and parse newline-separated commands from input file."
  [file]
  (map parse-line (-> (slurp (io/resource (str "aoc22/" file))) str/split-lines)))

(defn next-state [state instr]
  (let [{:keys [x history-x]} state
        {:keys [opcode arg]} instr]
    (merge state
           (condp = opcode
             :noop {:history-x (conj history-x x)}
             :addx (let [next-x (+ x arg)]
                     {:x next-x
                      :history-x (conj history-x x next-x)})))))

(defn calc-strength [xs n]
  (* n (nth xs (dec n))))

(comment

  (->> (map parse-line ["noop" "addx 3" "addx -5"])
       (reduce next-state {:x 1 :history-x []}))

  :end)

;;; Answers!

(defn part-1
  "Run with bb -x aoc22.day10/part-1"
  [_]
  (prn
   (let [xs (->> (parse-input "day10.txt")
                 (reduce next-state {:x 1 :history-x []})
                 :history-x)]
     (reduce
      + 0
      (map (partial calc-strength xs) [20 60 100 140 180 220])))))

(defn part-2
  "Run with bb -x aoc22.day10/part-2"
  [_]
  (prn nil))
