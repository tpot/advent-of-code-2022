(ns aoc22.day11
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.test :as test]))

;;; Input parsing

(def operator-map
  {"+" :plus "-" :minus "*" :multiply "/" :divide})

(defn parse-long-list
  "Parse a comma+space separated list of longs."
  [s]
  (map parse-long (str/split s #", ")))

(test/is
 (= [1 2 3 4]
    (parse-long-list "1, 2, 3, 4")))

(defn parse-operation
  "Parse a monkeying around arithmetic expression."
  [s]
  (let [matches (re-matches #"new = old (.*) (.*)" s)
        [oper op] (rest matches)]
    {:operator (get operator-map oper)
     :operand (or (if (= op "old") :old) (parse-long op) op)}))

(test/is
 (= {:operator :plus :operand 1}
    (parse-operation "new = old + 1")))

(test/is
 (= {:operator :multiply :operand :old}
    (parse-operation "new = old * old")))

(def monkey-processor
  "Parse sequence of lines with a regex and parse captures."
  [{:key :monkey   :re #"Monkey (\d+):"                      :fn parse-long}
   {:key :items    :re #"  Starting items: (.*)"             :fn parse-long-list}
   {:key :op       :re #"  Operation: (.*)"                  :fn parse-operation}
   {:key :test     :re #"  Test: divisible by (.*)"          :fn parse-long}
   {:key :if-true  :re #"    If true: throw to monkey (.*)"  :fn parse-long}
   {:key :if-false :re #"    If false: throw to monkey (.*)" :fn parse-long}])

(defn parse-monkey-desc
  "Parse monkey description into a map."
  [desc]
  (apply merge
   (map (fn [p line] {(:key p) ((:fn p) (second (re-matches (:re p) line)))})
        monkey-processor (str/split-lines desc))))

#_(test/is
 (= {:monkey 0, :items (79 98),
     :op {:operator :multiply, :operand 19},
     :test 23, :if-true 2, :if-false 3}
    (parse-monkey-desc
"Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3")))

(defn parse-input
  "Parse paragraphs of monkey descriptions and return map of monkey
  numbers and their description."
  [file]
  (let [input (-> (slurp (io/resource (str "aoc22/" file))) (str/split #"\n\n"))]
    {:num_monkeys (count input)
     :monkeys (->> input
                   (map parse-monkey-desc)
                   (map (fn [m] [(:monkey m) (dissoc m :monkey)]))
                   (into {}))}))

(map parse-input ["day11-sample.txt" "day11.txt"])

;;; Helpers

(defn eval-operation-fn
  "Evaluate an operation on your worry level."
  [operator operand]
  (fn [old-worry-level]
    (let [fn (condp = operator :plus + :minus - :multiply * :divide /)]
      (fn old-worry-level (if (= operand :old) old-worry-level operand)))))

(test/is
 (= 3 ((eval-operation-fn :plus 1) 2)))

(test/is
 (= 4 ((eval-operation-fn :plus :old) 2)))

(defn eval-throw-test-fn
  "Evaluate a divisibility test for a monkey throwing an item."
  [divisor]
  (fn [worry-level]
    (= (mod worry-level divisor) 0)))

;; Test: divisible by 2
(test/is
 (= [true false true false] (map #((eval-throw-test-fn 2) %) (range 4))))

(defn eval-throw-destination
  "Evaluate which monkey to throw an item to based on a divisibility test"
  [test-fn dest-if-false dest-if-true]
  (fn [worry-level]
    (if (test-fn worry-level) dest-if-false dest-if-true)))

;; Test: divisible by 2
;;   If true: throw to monkey 3
;;   If false: throw to monkey 4
(test/is
 (= [3 4 3 4]
    (map (partial (eval-throw-destination (eval-throw-test-fn 2) 3 4)) (range 4))))

(defn inspect-item
  "Determine the effect of `monkey` inspecting an item with worry
  level `item`."
  [monkey item]
  (let [operator (get-in monkey [:op :operator])
        operand (get-in monkey [:op :operand])
        operation-fn (eval-operation-fn operator operand)
        test-fn (eval-throw-test-fn (:test monkey))]
    (let [new-worry-level (quot (operation-fn item) 3)
          test-result (test-fn new-worry-level)
          target (if test-result (:if-true monkey) (:if-false monkey))]
      {:item new-worry-level :target target})))

(test/is
 (= {:item 500 :target 3}
    (inspect-item {:op {:operator :multiply :operand 19}
                   :test 3 :if-true 2 :if-false 3} 79)))

(defn map-vals [f coll]
  (zipmap (keys coll) (map f (vals coll))))

(defn assoc-ins
  "Like assoc-in but for multiple pairs of keys and values."
  [coll & kvs]
  (reduce
   (fn [result input] (apply assoc-in result input))
   coll (partition 2 kvs)))

(defn do-round
  "Return monkey state after a round of item throwing."
  [initial-state]
  ;; Loop over monkeys
  (loop [monkeys (range (get initial-state :num_monkeys))
         state initial-state]
    (let [ndx (first monkeys)
          monkey (get-in state [:monkeys ndx])
          ;; Determine list of items thrown by this monkey
          throw-list (map (partial inspect-item monkey) (:items monkey))
          ;; Group by target monkey
          throws-by-target (map-vals #(map :item %) (group-by :target throw-list))
;;          _ (println throws-by-target)
          ;; Keys and vals of target monkey items
          assoc-kvs (map (fn [target] [:monkeys target :items]) (keys throws-by-target))
          assoc-vals (map (fn [[target items]] (concat (get-in state [:monkeys target :items]) items)) throws-by-target)
          ;; Update state to delete from thrower and add to throwee
;;          _ (println (mapcat vector assoc-kvs assoc-vals))
          next-state (apply assoc-ins state
                            [:monkeys ndx :items] []
                            [:inspect-counts ndx] (+ (count (:items monkey)) (get-in state [:inspect-counts ndx]))
                            (mapcat vector assoc-kvs assoc-vals))]
      (if-not (next monkeys) next-state (recur (next monkeys) next-state)))))

(defn do-rounds [initial-state rounds]
  (let [num_monkeys (get initial-state :num_monkeys)
        state (assoc initial-state :inspect-counts (into [] (take num_monkeys (repeat 0))))]
    (reduce
     (fn [result _] (conj result (do-round (last result))))
     [state] (range rounds))))

(comment

  (let [initial-state (parse-input "day11-sample.txt")]
    (do-round initial-state))

  :end)

#_(test/is
 #_(= {0 {:items []}}
    (do-round {0 {:items []}}))
 (= [[] [] [] [6]]
    (->> {0 {:items [1]
             :op {:operator :multiply :operand 19}
             :test 23
             :if-true 2 :if-false 3}
          1 {:items []}
          2 {:items []}}
         do-round
         get-items)))

;;; Working

(comment

  (defn get-items [state]
    (map #(get-in state [:monkeys % :items]) (range (get state :num_monkeys))))

  (test/is
   (= [[20, 23, 27, 26] [2080, 25, 167, 207, 401, 1046] [] []]
      (get-items
       (let [initial-state (parse-input "day11-sample.txt")
             num_monkeys (get initial-state :num_monkeys)
             state (assoc initial-state :inspect-counts (into [] (take num_monkeys (repeat 0))))]
         (do-round state)))))

  :end)

;;; Answers!

(defn part-1
  "Run with bb -x aoc22.day11/part-1"
  [_]
  (prn
   (->> (do-rounds (parse-input "day11.txt") 20)
        last
        :inspect-counts
        sort
        reverse
        (take 2)
        (reduce * 1))))

(defn part-2
  "Run with bb -x aoc22.day11/part-2"
  [_]
  (prn nil))
