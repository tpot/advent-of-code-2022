(ns aoc22.day14
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.test :as t]))

;;; Input parsing

(defn parse-line [s]
  (->> (-> s (str/split #" -> "))
       (mapcat #(str/split % #","))
       (map parse-long)
       (partition 2)))

(defn parse-string [s]
  (->> (str/split-lines s) (map parse-line)))

(defn parse-file [file]
  (-> (slurp (io/resource (str "aoc22/" file))) parse-string))

(t/deftest parse-string-test
  (t/testing "String parsing"
    (t/is
     (= [[[498 4] [498 6] [496 6]] [[503 4] [502 4] [502 9] [494 9]]]
        (parse-string "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9")
        ))))

;; Line drawing

(defn line-points
  "Return the list of points connecting the coordinates of two nodes."
  [a b]
  (let [[x1 y1] a [x2 y2] b]
    (if (= x1 x2)
      (map vector
           (repeat x1)
           (map (partial + (min y1 y2)) (range (inc (abs (- y1 y2))))))
      (map vector
           (map (partial + (min x1 x2)) (range (inc (abs (- x1 x2)))))
           (repeat y1)))))

(t/deftest line-points-test
  (t/testing "Convert two nodes into list of points"
    ;; Forwards x-axis
    (t/is (= (set [[8 10] [9 10] [10 10]])
             (set (line-points [8 10] [10 10]))))
    ;; Backwards x-axis
    (t/is (= (set [[8 10] [9 10] [10 10]])
             (set (line-points [10 10] [8 10]))))
    ;; Forwards y-axis
    (t/is (= (set [[10 8] [10 9] [10 10]])
             (set (line-points [10 8] [10 10]))))
    ;; Backwards y-axis
    (t/is (= (set [[10 8] [10 9] [10 10]])
             (set (line-points [10 10] [10 8]))))))

(defn draw-line
  "Return the list of points connecting a list of nodes. Does not
     connect the start point to the end point."
  [nodes]
  (->> (partition 2 1 nodes)
       (mapcat (fn [[a b]] (line-points a b)))))

(t/deftest draw-line-test
  (t/testing "Converting list of nodes into list of points connecting them"
    ;; Three sides of a square
    (t/is (= (set [[0 0] [0 1] [0 2] [0 2] [1 2] [2 2] [2 0] [2 1] [2 2]])
             (set (draw-line [[0 0] [0 2] [2 2] [2 0]]))))
    ;; Sample data
    (t/is (= (set (mapcat draw-line (parse-file "day14-sample.txt")))
             (set [[498 4] [498 5] [498 6]
                   [498 6] [497 6] [496 6]
                   [503 4] [502 4]
                   [502 4] [502 5] [502 6] [502 7] [502 8] [502 9]
                   [502 9] [501 9] [500 9] [499 9] [498 9] [497 9] [496 9] [495 9] [494 9]])))))

;;; Helpers

;; Pieces on the board

(def piece-keys [:empty-piece :rock-piece :sand-piece])
(def piece-vals ["."          "#"         "o"])

(def piece-map (merge (zipmap piece-keys piece-vals) (zipmap piece-vals piece-keys)))

(defn point-down       [[x y]] [x       (inc y)])
(defn point-down-left  [[x y]] [(dec x) (inc y)])
(defn point-down-right [[x y]] [(inc x) (inc y)])

(defn blocked-down?       [ps p] (get ps (point-down       p)))
(defn blocked-down-left?  [ps p] (get ps (point-down-left  p)))
(defn blocked-down-right? [ps p] (get ps (point-down-right p)))

(defn bounding-box [ps]
  (let [[x1 x2] [(apply min (map first ps)) (apply max (map first ps))]
        [y1 y2] [(apply min (map second ps)) (apply max (map second ps))]]
    [[x1 y1] [x2 y2]]))

(defn plot->str [xs]
  (str/join "\n" (map (partial str/join "") xs)))

(defn plot
  [pcoll]
  (let [ps (apply set/union (vals pcoll))
        [[min-x min-y] [max-x max-y]] (bounding-box ps)
        [range-x range-y] [(- max-x min-x) (- max-y min-y)]
        [size-x size-y] [(inc range-x) (inc range-y)]
        empty-board (into [] (repeat (* size-x size-y) (get piece-map :empty-piece)))
        xf1 (fn [[x y]] (+ x (* size-x y)))
        xf2 (fn [[x y]] [(- x min-x) (- y min-y)])
        xf (comp xf1 xf2)]
    (partition
     size-x
     (reduce
      (fn [board [piece-name p]] (assoc board (xf p) (get piece-map piece-name)))
      empty-board
      (mapcat (fn [[k v]] (map vector (repeat k) v)) pcoll)))))

(defn fall
  "Return furthest point sand can fall or nil for falling into the void."
  [ps p]
  (let [max-y (apply max (map second ps))]
    (if (<= (second p) max-y)
      ;; Move down if possible
      (if-not (blocked-down? ps p)
        (fall ps (point-down p))
        ;; Move left if possible
        (if-not (blocked-down-left? ps p)
          (fall ps (point-down-left p))
          ;; Move right if possible
          (if-not (blocked-down-right? ps p)
            (fall ps (point-down-right p))
            p)))
      ;; Fell off!
      nil)))

(def initial-sand-pos [500 0])

(defn drop-sand [rock-points]
  (loop [sand-points #{}]
    (let [sand-pos (fall (set/union rock-points sand-points) initial-sand-pos)]
      (if-not sand-pos sand-points (recur (conj sand-points sand-pos))))))

;;; Answers!

(t/deftest sample-data-part-1
  (t/testing "Check result for sample data"
    (t/is
     (= 24
        (let [rock-points (set (mapcat draw-line (parse-file "day14-sample.txt")))
              sand-points (drop-sand rock-points)
              result {:rock-piece rock-points :sand-piece sand-points}]
          (count sand-points))))))

(defn part-1
  "Run with bb -x aoc22.day14/part-1"
  [_]
  (prn
   (let [rock-points (set (mapcat draw-line (parse-file "day14.txt")))
         sand-points (drop-sand rock-points)
         result {:rock-piece rock-points :sand-piece sand-points}]
     (count sand-points)))) ;; => 885

(defn part-2
  "Run with bb -x aoc22.day14/part-2"
  [_]
  (prn nil))

(t/run-tests)
