(ns aoc22.day13
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.test :as t]))

;;; Input parsing

(defn parse-string [s]
  (->> (-> s (str/split #"\n\n"))
       (map str/split-lines)
       (map #(map read-string %))))

(defn parse-file [file]
  (->> (-> (slurp (io/resource (str "aoc22/" file))) parse-string)))

(t/deftest parse-string-test
  (t/testing "String parsing"
    (t/is
     (= '(([[1 2] [3 4]] [[5 6] [7 8]]))
        (parse-string "[[1,2],[3, 4]]\n[[5,6],[7,8]]")))))

;;; Helpers

;; Determine whether two integers are in order

(defn in-order-ints?
  "If both values are integers, the lower integer should come first."
  [x y]
  (if (= x y) :no-decision (< x y)))

(t/deftest in-order-ints-test
  (t/testing "Both values are integers"
    (t/is (= true (in-order-ints? 1 2)))
    (t/is (= :no-decision (in-order-ints? 1 1)))
    (t/is (= false (in-order-ints? 2 1)))))

;; Determine whether two lists are in order

(declare in-order?)

(defn in-order-lists?
  "If both values are lists, compare the first value of each list,
  then the second value, and so on. If the left list runs out of
  items first, the inputs are in the right order. If the right list
  runs out of items first, the inputs are not in the right order. If
  the lists are the same length and no comparison makes a decision
  about the order, continue checking the next part of the input."
  [xs ys]
  (reduce
   (fn [result [x y]]
     (cond
       ;; Left run out of values
       (and (= (count x) 0) (> (count y) 0))
       (reduced true)
       ;; Right run out of values
       (and (> (count x) 0) (= (count y) 0))
       (reduced false)
       ;; Both run out of values
       (and (= (count x) 0) (= (count y) 0))
       :no-decision
       ;; Compare values
       :else
       (let [result (in-order? (first x) (first y))]
         (if (= result :no-decision)
           (in-order? (rest x) (rest y))
           result))))
   :no-decision [[xs ys]]))

(t/deftest in-order-lists-test
  (t/testing "Both values are lists"
    (t/is (= true (in-order-lists? [1] [2])))
    (t/is (= false (in-order-lists? [2] [1])))
    (t/is (= true (in-order-lists? [] [1])))
    (t/is (= false (in-order-lists? [1] [])))))

;; Top level function to determine whether two things are in order

(defn in-order?
  "Return true if x and y are in the right order, nil if no decision
  can be made."
  [x y]
  (cond
    ;;  If both values are integers, the lower integer should come
    ;;  first.
    (and (integer? x) (integer? y))
    (in-order-ints? x y)
    ;; If both values are lists, determine whether they are in order
    (and (sequential? x) (sequential? y))
    (in-order-lists? x y)
    ;; If one value is an integer convert to a list
    (integer? x)
    (in-order-lists? (vector x) y)
    (integer? y)
    (in-order-lists? x (vector y))))

(t/deftest in-order-ints-test
  (t/testing "Both values are integers"
    (t/is (= true (in-order-ints? 1 2)))
    (t/is (= :no-decision (in-order-ints? 1 1)))
    (t/is (= false (in-order-ints? 2 1)))))

;; Testing sample data

(t/deftest sample-data
  (t/testing "Test sample data"
    (t/is (= true (in-order? [1 1 3 1 1] [1 1 5 1 1])))
    (t/is (= true (in-order? [[1] [2 3 4]] [[1] 4])))
    (t/is (= false (in-order? [9] [[8 7 6]])))
    (t/is (= true (in-order? [[4 4] 4 4] [[4 4] 4 4 4])))
    (t/is (= false (in-order? [7 7 7 7] [7 7 7])))
    (t/is (= true (in-order? [] [3])))
    (t/is (= false (in-order? [[[]]] [[]])))
    (t/is (= false (in-order? [1 [2 [3 [4 [5 6 7]]]] 8 9] [1 [2 [3 [4 [5 6 0]]]] 8 9])))))

;;; Answers!

(defn part-1
  "Run with bb -x aoc22.day13/part-1"
  [_]
  (prn
   (->> (parse-file "day13.txt")
        (keep-indexed #(if (apply in-order? %2) (inc %1)))
        (reduce + 0))))

(defn part-2
  "Run with bb -x aoc22.day13/part-2"
  [_]
  (prn nil))

(t/run-tests)
