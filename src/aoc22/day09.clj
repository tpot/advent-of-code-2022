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
  [head-pos tail-pos dir]
  (let [move-vec (get move-vec dir)
        next-head-pos (next-head-pos head-pos dir)
        head-delta (map abs (map - next-head-pos tail-pos))]
    (cond
      ;; No movement between head and tail, tail doesn't move
      (= head-delta [0 0])
      tail-pos
      ;; Only moved one step away (incl. diagonal), tail doesn't move
      (every? true? (map #(or (zero? %) (one? %)) head-delta)) ; ugh
      tail-pos
      ;; If moved two steps in a direction, tail catches up
      (= (map (comp abs (partial * 2)) move-vec) head-delta)
      (map + tail-pos move-vec)
      ;; If moved two steps diagonally, tail moves diagonally to catch up
      :else
      (let [[tx ty] tail-pos
            [hx hy] next-head-pos]
        [(if (> hx tx) (inc tx) (dec tx))
         (if (> hy ty) (inc ty) (dec ty))])))) ; ugh

(defn next-state
  "Calculate the next state given previous state and an operation."
  [state op]
  (let [[dir count] op]
    (loop [state state
           count count]
      (let [{:keys [head-pos tail-pos]} state
            next-head-pos (next-head-pos head-pos dir)
            next-tail-pos (next-tail-pos head-pos tail-pos dir)
            next-state (assoc
                        (merge state {:head-pos next-head-pos :tail-pos next-tail-pos})
                        :prev-state (conj (:prev-state state) {:head-pos head-pos :tail-pos tail-pos :dir dir}))]
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

  ;; Tests

  (def sample-moves
    [;; Head covering tail
     {:head-pos [0 0] :tail-pos [0 0] :dir :right
      :next-head-pos [1 0] :next-tail-pos [0 0]}
     ;; Two steps away in single direction
     {:head-pos [1 0] :tail-pos [0 0] :dir :right
      :next-head-pos [2 0] :next-tail-pos [1 0]}
     {:head-pos [0 0] :tail-pos [0 1] :dir :down
      :next-head-pos [0 -1] :next-tail-pos [0 0]}
     ;; Sample tail single direction moves
     {:head-pos [0 0] :tail-pos [-1 0] :dir :right
      :next-head-pos [1 0] :next-tail-pos [0 0]}
     {:head-pos [0 0] :tail-pos [0 1] :dir :down
      :next-head-pos [0 -1] :next-tail-pos [0 0]}
     ;; Sample tail diagonal moves
     {:head-pos [0 0] :tail-pos [-1 -1] :dir :up
      :next-head-pos [0 1] :next-tail-pos [0 0]}
     {:head-pos [0 0] :tail-pos [-1 -1] :dir :right
      :next-head-pos [1 0] :next-tail-pos [0 0]}])

  (defn test-move [d]
    (let [dir (:dir d)
          {:keys [head-pos tail-pos]} d
          expected-head-pos (:next-head-pos d)
          expected-tail-pos (:next-tail-pos d)
          actual-head-pos (next-head-pos head-pos dir)
          actual-tail-pos (next-tail-pos head-pos tail-pos dir)
          ok-head (= actual-head-pos expected-head-pos)
          ok-tail (= actual-tail-pos expected-tail-pos)]
      (if (and ok-head ok-tail)
        nil
        (merge {:dir dir :head-pos head-pos :tail-pos tail-pos}
               (if-not ok-head {:expected-head-pos expected-head-pos :actual-head-pos actual-head-pos})
               (if-not ok-tail {:expected-tail-pos expected-tail-pos :actual-tail-pos actual-tail-pos})))))

  (filter coll? (map test-move sample-moves))

  (next-state {:head-pos [0 0] :tail-pos [0 0]} [:right 2])

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

;; 6493 too low

(defn part-2
  "Run with bb -x aoc22.day09/part-2"
  [_]
  (prn nil))
