(ns aoc22.day08
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.set :as set]))

;;; Input parsing

(defn parse-input
  "Convert newline-separated single digits into a matrix."
  [file]
  (map
   (fn [line] (->> line seq (map int) (map #(- % (int \0)))))
   (-> (slurp (io/resource (str "aoc22/" file))) str/split-lines)))

;;; Helpers

(defn matrix-transpose [m]
  (map
   (fn [col] (map #(nth % col) m))
   (range (count (first m)))))

(defn matrix-reverse [m]
  (map reverse m))

;; Helpers for part 1

(defn index-visible-row
  "Return indexes of trees visible in a row, from left to right. A
  tree is visible if it is higher than the highest tree so far."
  [xs]
  (:visible
   (reduce
    (fn [result ndx]
      (if (> (nth xs ndx) (:highest result))
        {:highest (nth xs ndx) :visible (conj (:visible result) ndx)}
        result))
    {:highest (first xs) :visible [0]} (range (count xs)))))

(defn coords-visible
  "Return coordinates of visible trees. (Indexes are one-dimensional
  coordinates."
  [m]
  (apply concat
         (for [row (range (count m))]
           (map (partial vector row) (index-visible-row (nth m row))))))

;; Helpers for part 2

(defn visible
  "Determine how many trees are visible from a tree of a given height."
  [height trees]
  (reduce
   (fn [result tree]
     (if (>= tree height) (reduced (inc result)) (inc result)))
   0 trees))

(defn score [distances]
  (apply * distances))

;;; Working

(comment

  ;;; Part 1

  ;; Visibility from left to right (observer looking east)
  (coords-visible m)
  ;; => ([0 0] [0 3] [1 0] [1 1] [2 0] [3 0] [3 2] [3 4] [4 0] [4 1] [4 3])

  ;; Visibility from right to left (observer looking west)
  (map (fn [[x y]] [x (- 4 y)])
       (coords-visible (matrix-reverse m)))
  ;; => ([0 4] [0 3] [1 4] [1 2] [2 4] [2 3] [2 1] [2 0] [3 4] [4 4] [4 3])

  ;; Visibility from up to down (observer looking south)
  (map (fn [[x y]] [y x])
       (coords-visible (matrix-transpose m)))
  ;; => ([0 0] [2 0] [0 1] [1 1] [0 2] [1 2] [0 3] [4 3] [0 4] [3 4])

  ;; Visibility from down to up (observer looking north)
  (map (fn [[x y]] [(- 4 y) x])
       (coords-visible (matrix-reverse (matrix-transpose m))))
  ;; => ([4 0] [2 0] [4 1] [4 2] [3 2] [4 3] [4 4] [3 4])

  ;;; Part 2

  ;; 3 0 3 7 3
  ;; 2 5 5 1 2
  ;; 6 5 3 3 2
  ;; 3 3 5 4 9
  ;; 3 5 3 9 0

  ;; Techinque - reuse idea of running the sanme calculation function
  ;; on reversed, transposed, and reversed and transposed input
  ;; data. Unfortunately I don't think a lot of actual code is going
  ;; to be reused which might indicate I did not do the first part the
  ;; right way.

  ;; Generate lists of trees to the north, south, east and west of the
  ;; current tree. Count the number of trees until reaching a tree is
  ;; the same or higher height.

  (reduce
   max 0
   (let [m (parse-input "day08-sample.txt")
         t (matrix-transpose m)
         r (count m)] ; rank
     (for [row-ndx (range (count m))
           col-ndx (range (count (first m)))]
       (let [row (nth m row-ndx)
             col (nth t col-ndx)
             tree (nth row col-ndx)]
         (let [east  (drop (+ col-ndx 1) row)
               south (drop (+ row-ndx 1) col)
               west  (reverse (take col-ndx row))
               north (reverse (take row-ndx col))]
           (->> [east south west north]
                (map (partial visible tree))
                score))))))

  ;; Stop reducing when we see a tree the same height or taller than
  ;; the tree under consideration.

  (map (partial visible 5)
       [[3] [5 2] [1 2] [3 5 3]]) ; => (1 1 2 2)

  (score [1 1 2 2]) ; => 4

  :end)

;;; Answers!

(defn part-1
  "Run with bb -x aoc22.day08/part-1"
  [_]
  (prn
   (let [m (parse-input "day08.txt")
         r (- (count m) 1)] ; rank
     (let [coords-lr (coords-visible m)
           coords-rl (coords-visible (matrix-reverse m))
           coords-ud (coords-visible (matrix-transpose m))
           coords-du (coords-visible (matrix-reverse (matrix-transpose m)))]
       (count
        (set
         (concat
          (map (fn [[x y]] [x       y ]) coords-lr)
          (map (fn [[x y]] [x  (- r y)]) coords-rl)
          (map (fn [[x y]] [y       x ]) coords-ud)
          (map (fn [[x y]] [(- r y) x ]) coords-du))))))))

(defn part-2
  "Run with bb -x aoc22.day08/part-2"
  [_]
  (prn
   (reduce
    max 0
    (let [m (parse-input "day08.txt")
          t (matrix-transpose m)
          r (count m)] ; rank
      (for [row-ndx (range (count m))
            col-ndx (range (count (first m)))]
        (let [row (nth m row-ndx)
              col (nth t col-ndx)
              tree (nth row col-ndx)]
          (let [east  (drop (+ col-ndx 1) row)
                south (drop (+ row-ndx 1) col)
                west  (reverse (take col-ndx row))
                north (reverse (take row-ndx col))]
            (->> [east south west north]
                 (map (partial visible tree))
                 score))))))))
