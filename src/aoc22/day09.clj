(ns aoc22.day09
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.set :as set]))

;;; Input parsing

(def dirs {"L" :left, "R" :right, "U" :up, "D" :down})

(defn parse-line
  "Parse a single line from input file."
  [line]
  (let [[dir dist] (str/split line #" ")]
    [(dirs dir) (parse-long dist)]))

(defn parse-input
  "Read and parse newline-separated commands from input file."
  [file]
  (map parse-line (-> (slurp (io/resource (str "aoc22/" file))) str/split-lines)))

;;; Helpers

(defn one? [n] (= n 1))

(defn x [v] (first v))
(defn y [v] (second v))

(defn clamp [v] (map #(-> % (max -1) (min 1)) v))

(def move-vec
  "Map direction to coordinate vector."
  {:left  [-1 0] :right [1  0]
   :up    [0  1] :down  [0  -1]})

(defn next-head-pos
  "Calculate next head position."
  [head-pos dir]
  (map + head-pos (get move-vec dir)))

(defn next-tail-pos
  "Calculate next tail position."
  [head-pos tail-pos]
  (let [[hx hy] head-pos
        [tx ty] tail-pos
        head-delta (map - head-pos tail-pos)]
    (cond
      ;; No movement between head and tail, tail doesn't move
      (= head-delta [0 0])
      tail-pos
      ;; Only moved one step away (incl. diagonal), tail doesn't move
      (every? (partial not= 2) (map abs head-delta))
      tail-pos
      ;; If moved two steps in a direction, tail catches up
      (and (= tx hx) (= (- ty hy) 2))
      [tx (dec ty)]
      (and (= tx hx) (= (- ty ty) -2))
      [tx (inc ty)]
      (and (= ty hy) (= (- tx hx) 2))
      [(dec tx) ty]
      (and (= ty hy) (= (- tx hx) -2))
      [(inc tx) ty]
      ;; If moved two steps diagonally, tail moves diagonally to catch up
      (or (= (abs (x head-delta)) 2) (= (abs (y head-delta)) 2))
      (map + (clamp head-delta) tail-pos))))

(defn next-state
  "Calculate the next state given previous state and an operation."
  [state op]
  (let [[dir count] op]
    (loop [state state
           count count]
      (let [{:keys [head-pos tail-pos]} state
            next-head-pos (next-head-pos head-pos dir)
            next-tail-pos (next-tail-pos next-head-pos tail-pos)
            next-state (assoc
                        (merge state {:head-pos next-head-pos :tail-pos next-tail-pos})
                        :prev-state (conj
                                     (:prev-state state)
                                     {:prev-head-pos head-pos :prev-tail-pos tail-pos
                                      :head-pos next-head-pos :tail-pos next-tail-pos :dir dir}))]
        (if (= count 1) next-state (recur next-state (dec count)))))))

;;; Working

(comment

  ;; Exploration and debugging

  (defn board->str
    "Convert a board to a string."
    [board]
    (str/join
     "\n"
     (map (partial str/join "")
          (reverse (partition 5 board)))))

  (defn set-board
    "Set a value at a coordinate on a board."
    [board coord x]
    (assoc board (+ (+ 2 (first coord)) (* 5 (+ 2 (second coord)))) x))

  (defn make-board
    "Create a board with a specified head and tail position."
    [head-pos tail-pos]
    (let [board (into [] (repeat 25 "."))]
      (-> board
          (set-board tail-pos "T")
          (set-board head-pos "H"))))

  (println
   (board->str (make-board [0 0] [1 1])))

  (let [input (parse-input "day09-sample.txt")]
    (->> input
         (reduce next-state {:head-pos [0 0] :tail-pos [0 0] :prev-state []})
         :prev-state
         (map :tail-pos)
         set
         count)) ;; => 13

  :end)

;;; Answers!

(defn part-1
  "Run with bb -x aoc22.day09/part-1"
  [_]
  (prn
   (let [input (parse-input "day09.txt")]
     (->> input
          (reduce next-state {:head-pos [0 0] :tail-pos [0 0] :prev-state []})
          :prev-state
          (map :tail-pos)
          set
          count))))

(defn part-2
  "Run with bb -x aoc22.day09/part-2"
  [_]
  (prn nil))
