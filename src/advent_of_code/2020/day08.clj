(ns advent-of-code.2020.day08
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code.utils :refer :all]))

(defn parser [content]
  (->> (re-seq #"(\w+) (\+\d+|\-\d+)" content)
       (map #(hash-map :op (nth % 1) :n (read-string (nth % 2))))))

(defn answer-1 []
  (let [reference (parser (get-content "2020/day08-input.txt"))]
    (loop [index 0
           acc 0
           used #{}]
      (when-let [{:keys [op n]} (nth reference index)]
        (cond
          (contains? used index) acc
          (= "acc" op) (recur (+ index 1) (+ acc n) (conj used index))
          (= "nop" op) (recur (+ index 1) acc (conj used index))
          (= "jmp" op) (recur (+ index n) acc (conj used index)))))))

(declare terminates?)

(defn acc [reference index n tally used]
  (terminates? reference (+ index 1) (+ tally n) (conj used index)))

(defn jmp [reference index n tally used]
  (terminates? reference (+ index n) tally (conj used index)))

(defn nop [reference index n tally used]
  (terminates? reference (+ index 1) tally (conj used index)))

(defn terminates? [reference index tally used]
  (if (<= (count reference) index)
    tally
    (when-let [{:keys [op n]} (nth reference index)]
      (cond
        (contains? used index) nil
        (= "acc" op) (acc reference index n tally used)
        (= "nop" op) (or (nop reference index n tally used)
                         (jmp reference index n tally used))
        (= "jmp" op) (or (jmp reference index n tally used)
                         (nop reference index n tally used))))))

(defn answer-2 []
  (let [reference (parser (get-content "2020/day08-input.txt"))]
    (terminates? reference 0 0 #{})))
