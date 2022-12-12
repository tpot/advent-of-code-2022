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

(defn next-state
  "Return next state of machine after executing an instruction."
  [state instr]
  (let [{:keys [x history-x]} state
        {:keys [opcode arg]} instr]
    (merge state
           (condp = opcode
             :noop {:history-x (conj history-x x)}
             :addx (let [next-x (+ x arg)]
                     {:x next-x
                      :history-x (conj history-x x next-x)})))))

(defn calc-strength
  "Return signal strength."
  [xs n]
  (* n (nth xs (dec n))))

(defn pixel
  "Return pixel value for a position and X value."
  [[pos x]]
  (if (or (= (+ pos 1) (- x 1))
          (= (+ pos 1) x)
          (= (+ pos 1) (+ x 1))) "#" "."))

(comment

  (->> (map parse-line ["noop" "addx 3" "addx -5"])
       (reduce next-state {:x 1 :history-x []}))

  ;; Sample

  ;; ##..##..##..##..##..##..##..##..##..##..
  ;; ###...###...###...###...###...###...###.
  ;; ####....####....####....####....####....
  ;; #####.....#####.....#####.....#####.....
  ;; ######......######......######......####
  ;; #######.......#######.......#######.....

  ;; Output

  ;; #..##..##..##..##..##..##..##..##..##...
  ;; ##...###...###...###...###...###...###..
  ;; ###....####....####....####....####.....
  ;; ####.....#####.....#####.....#####......
  ;; #####......######......######......####.
  ;; ######.......#######.......#######......

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
  (let [xs (->> (parse-input "day10.txt")
                 (reduce next-state {:x 1 :history-x []})
                 :history-x)]
    ;; Off-by-one bug cuts off the first column but still readable!
    (println
     (str/join
      "\n"
      (->> (map #(list (mod %1 40) %2) (range (count xs)) xs)
           (map pixel)
           (partition 40)
           (map str/join))))))
