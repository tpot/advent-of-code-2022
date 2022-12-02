(ns aoc22.day02
  (:require #?(:clj [clojure.java.io :as io]
               :cljs [nbb.core :refer [slurp await]])
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            #?(:cljs [promesa.core :as p])))

;; Read and parse input
#?(:clj
   (def input (->> (slurp (io/resource "aoc22/day02.txt"))
                   str/split-lines
                   (map #(str/split % #" "))))
   :cljs
   (def input (await (p/->> (slurp "resources/aoc22/day02.txt")
                            str/split-lines
                            (map #(str/split % #" "))))))

;; Encoding of strategy guide
(def play     {"A" :rock      "B" :paper     "C" :scissors})
(def response {"X" :rock      "Y" :paper     "Z" :scissors}) ; part-1
(def desired  {"X" :need-loss "Y" :need-draw "Z" :need-win}) ; part-2

;; Score play response and round result
(def score {:rock 1 :paper 2 :scissors 3
            :loss 0 :draw  3 :win      6})

;; Given opponent play and our response, return round result
(def result-matrix {:rock     {:rock     :draw
                               :paper    :win
                               :scissors :loss}
                    :paper    {:rock     :loss
                               :paper    :draw
                               :scissors :win}
                    :scissors {:rock     :win
                               :paper    :loss
                               :scissors :draw}})

(defn play-result [play response]
  (get-in result-matrix [play response]))

;; Given opponent play and desired result, return response
(def desired-response-matrix
  {:rock      {:need-loss :scissors
               :need-draw :rock
               :need-win  :paper}
   :paper     {:need-loss :rock
               :need-draw :paper
               :need-win  :scissors}
   :scissors {:need-loss :paper
              :need-draw :scissors
              :need-win  :rock}})

(defn desired-response [play desired-result]
  (get-in desired-response-matrix [play desired-result]))

;; Score a round
(defn score-round-part1
  "Score a round interpreting X, Y, Z as rock, paper, scissors."
  [play response]
  (let [result (play-result play response)]
    (+ (get score response) (get score result))))

(defn score-round-part2
  "Score a round interpreting X, Y, Z as need loss, need draw, need win."
  [play desired-result]
  (let [response (desired-response play desired-result)
        result (play-result play response)]
    (+ (get score response) (get score result))))

(comment

  (play-result :rock :paper) ; => :win
  (score-round-part1 :rock :rock) ; => 4

  (->> [[:rock :paper] [:paper :rock] [:scissors :scissors]]
       (map #(score-round-part1 (first %) (second %)))) ; => (8 1 6)

  (->> [[:rock :need-draw] [:paper :need-loss] [:scissors :need-win]]
       (map #(score-round-part2 (first %) (second %)))) ; => (4 1 7)

  :end)

(defn part-1
  "Run with (n)bb -x aoc22.day02/part-1"
  [_]
  (prn
   (->> input
        (map (fn [[x y]] [(get play x) (get response y)]))
        (map #(score-round-part1 (first %) (second %)))
        (reduce +))))

(defn part-2
  "Run with (n)bb -x aoc22.day02/part-2"
  [_]
  (prn
   (->> input
        (map (fn [[x y]] [(get play x) (get desired y)]))
        (map #(score-round-part2 (first %) (second %)))
        (reduce +))))
