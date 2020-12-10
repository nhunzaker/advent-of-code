(ns advent-of-code.2020.day09
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code.utils :refer :all]))

(defn corruption [list]
  (let [focus (last list)
        combos (pairs (drop-last list))]
    (when-not (some #(= focus (sum %)) combos)
      focus)))

(defn seek-corruption [lines buffer-size]
  (->> lines
       (partition-all (inc buffer-size) 1)
       (some corruption)))

(defn worm
  ([comparator list]
   (worm comparator list []))
  ([comparator list numbers]
   (case (comparator numbers)
     :more (when-let [number (first list)]
             (recur comparator
                    (rest list)
                    (conj numbers number)))
     :same numbers
     :less (recur comparator
                  list
                  (subvec numbers 1)))))

(defn seek-weakness [corruption lines]
  (->> lines
       (worm (fn [numbers]
               (let [value (sum numbers)]
                 (cond
                   (> 2 (count numbers)) :more
                   (= corruption value) :same
                   (< corruption value) :less
                   :else :more))))))

(defn answer-1 []
  (let [lines (map read-string (get-lines "2020/day09-input.txt"))]
    (seek-corruption lines 25)))

(defn answer-2 []
  (let [lines (map read-string (get-lines "2020/day09-input.txt"))
        weakness (some-> (seek-corruption lines 25)
                         (seek-weakness lines))]
    (+ (apply min weakness)
       (apply max weakness))))
