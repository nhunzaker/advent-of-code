(ns advent-of-code.2020.day01
  (:require [clojure.java.io :as io]))

(defn read-numbers [file]
  (->> (io/reader file)
       (line-seq)
       (into #{} (map read-string))))

(defn find-match [total n number-pool & [tally]]
  (if (>= 1 n)
    (when (contains? number-pool total)
      (conj tally total))
    (->> number-pool
         (some #(when (>= total %)
                  (find-match (- total %)
                              (dec n)
                              (disj number-pool %)
                              (conj tally %)))))))


;; -------------------------------------------------- ;;


(def input (io/resource "day01-input.txt"))

(defn report [matches]
  (if matches
    (do
      (printf "%s = %s\n" (str/join " + " matches) (apply + matches))
      (printf "%s = %s\n" (str/join " * " matches) (apply * matches)))
    (println "No solution found.\n")))

(defn answer [n]
  (->> (read-numbers input)
       (find-match 2020 n)
       (report)))
