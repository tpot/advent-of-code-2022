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

(defn board->str
  [board]
  (str/join
   "\n"
   (map (partial str/join "")
        (partition (:x-len board) (:pieces board)))))

(defn set-board
  [board point piece]
  (let [x-pos (- (first point) (:x-offset board))
        y-pos (- (second point) (:y-offset board))]
    [x-pos y-pos]
    (assoc-in board [:pieces (+ x-pos (* (:x-len board) y-pos))] piece)))

(defn load-board [filename]
  (let [input (set (mapcat draw-line (parse-file filename)))]
    (let [[x1 x2] [(apply min (map first input)) (apply max (map first input))]
          [y1 y2] [(apply min (map second input)) (apply max (map second input))]
          x-len (inc (- x2 x1))
          y-len (inc (- y2 0))]
      (reduce
       (fn [board point]
         (set-board board point "#"))
       {:pieces (into [] (repeat (* x-len y-len) "."))
        :x-offset x1 :y-offset 0
        :x-len x-len :y-len y-len}
       input))))

(comment

  (do
    (println "---")
    (println (board->str (load-board "day14-sample.txt"))))

  :end)

;;; Answers!

(defn part-1
  "Run with bb -x aoc22.day14/part-1"
  [_]
  (prn nil))

(defn part-2
  "Run with bb -x aoc22.day14/part-2"
  [_]
  (prn nil))

(t/run-tests)
