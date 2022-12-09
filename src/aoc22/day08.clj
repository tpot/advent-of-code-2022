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

(defn matrix-transpose [m]
  (map
   (fn [col] (map #(nth % col) m))
   (range (count (first m)))))

(defn matrix-reverse [m]
  (map clojure.core/reverse m))

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
  "Coordinates of visible trees"
  [m]
  (apply concat
         (for [row (range (count m))]
           (map (partial vector row) (index-visible-row (nth m row))))))

(comment

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
  (prn nil))
