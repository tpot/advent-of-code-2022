(ns aoc22.day07
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.walk :as w]))

;;; Read input files

(defn read-input [file] (-> (slurp (io/resource (str "aoc22/" file))) str/split-lines))

(def input (read-input "day07.txt"))
(def sample-input (read-input "day07-sample.txt"))

;;; Parse input files

(defn parse-cmd
  "Parse a line describing a command, e.g
     $ ls
     $ cd /"
  [line]
  (let [parts (-> line (str/split #" "))]
    (if (= (first parts) "$")
      {:type :cmd
       :cmd (second parts)
       :args (rest (rest parts))})))

(defn parse-file
  "Parse a line describing a file, e.g
     14848514 b.txt
     8504156 c.dat"
  [line]
  (let [parts (-> line (str/split #" "))]
    (if-let [size (parse-long (first parts))]
      {:type :file
       :filename (second parts)
       :filesize  size})))

(defn parse-dir
  "Parse a line describing a directory, e.g
     dir a
     dir a"
  [line]
  (let [parts (-> line (str/split #" "))]
    (if (= (first parts) "dir")
      {:type :dir
       :dirname (second parts)})))

(defn parse-lines
  "Parse a seq of input lines"
  [lines]
  (map #(or (parse-file %) (parse-dir %) (parse-cmd %)) lines))

;;; Update state based on input received

;; Commands

(defn next-state-cmd-cd
  [state op]
  (let [cwd (:cwd state)
        target (first (:args op))]
    (cond
      ;; Change to root
      (= target "/")
      (assoc state :cwd ["/"])
      ;; Change up to parent
      (= target "..")
      (assoc state :cwd (if (> (count cwd) 1) (pop cwd) cwd))
      ;; Change down to subdir
      :else
      (assoc state :cwd (conj cwd target)))))

(defn next-state-cmd-ls [state op]
  (assoc-in state (concat [:fs] (:cwd state)) {}))

(defn next-state-cmd [state op]
  (condp = (:cmd op)
    "cd" (next-state-cmd-cd state op)
    "ls" (next-state-cmd-ls state op)))

;; Dir entry

(defn next-state-dir [state op]
  state)

;; File entry

(defn next-state-file [state op]
  (let [{:keys [filename filesize]} op
        path (concat [:fs] (:cwd state) [filename])]
    (assoc-in state path filesize)))

(defn next-state [state op]
  (condp = (:type op)
    :cmd  (next-state-cmd state op)
    :dir  (next-state-dir state op)
    :file (next-state-file state op)))

;; Generate data for testing next state functions

(defn cd [path] {:type :cmd, :cmd "cd" :args [path]})
(defn ls []     {:type :cmd, :cmd "ls" :args []})

(defn file [name size] {:type :file, :filename name, :filesize size})
(defn dir  [name]      {:type :dir,  :dirname  name})

(comment

  (->> [(cd "/") (cd "tmp") (ls) (file "a" 1)(file "b" 2)]
       (reduce next-state {:cwd ["/"] :fs {}}))
  ;; => {:cwd ["/" "tmp"], :fs {"/" {"tmp" {"a" 1, "b" 2}}}

  :end)

;;; Helper functions

(def initial-state {:cwd ["/"] :fs {}})

(defn calc-dir-size
  "Enrich fs with directory size by walking fs state postorder"
  [fs]
  (w/postwalk
   (fn [x]
     (cond
       (map? x)
       (let [size (reduce + 0 (remove map? (vals x)))
             dsize (reduce + 0 (map :size (filter map? (vals x))))]
         (assoc x :size (+ size dsize)))
       :else x))
   fs))

(defn flatten-dirs
  "Return a seq of directories in a fs"
  [fs]
  (tree-seq
   (fn [node] (map? node))
   (fn [node] (filter map? (vals node)))
   fs))

;;; Answers!

(defn part-1
  "Run with bb -x aoc22.day07/part-1"
  [_]
  (prn
   (let [state (->> input
                    parse-lines
                    (reduce next-state initial-state))]
     (->> (calc-dir-size (:fs state))
          flatten-dirs
          (map :size)
          (filter #(<= % 100000))
          (reduce + 0)))))

(defn part-2
  "Run with bb -x aoc22.day02/part-2"
  [_]
  (prn
   (let [state (->> input
                    parse-lines
                    (reduce next-state initial-state))
         fs (calc-dir-size (:fs state))
         max-space 70000000
         used-space (get-in fs ["/" :size])
         free-space (- max-space used-space)
         required-space 30000000]
     (first
      (filter #(> % (- required-space free-space))
              (->> fs flatten-dirs (map :size) sort))))))
