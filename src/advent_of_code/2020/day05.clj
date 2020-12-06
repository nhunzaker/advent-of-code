(ns advent-of-code.2020.day05
  (:require [advent-of-code.utils :refer :all]
            [clojure.set :as set]))

(defn mid-point [min max]
  (int (* 0.5 (+ min max))))

;; Remember index at 0
(defn bin [size original-letters]
  (loop [letters original-letters
         left 0
         right size]
    (case (first letters)
      nil right
      (\F \L) (recur (rest letters)
                     left (mid-point left right))
      (\B \R) (recur (rest letters)
                     (mid-point left right) right))))

(defn seat->id [{:keys [row col]}]
  (+ (* row 8) col))

(defn find-seat [line-str]
  {:row (bin 127 (take 7 line-str))
   :col (bin 7 (drop 7 line-str))})

(defn answer-1 []
  (->> (get-lines "2020/day05-input.txt")
       (map (comp seat->id find-seat))
       (apply max)))

(defn answer-2 []
  (->> (get-lines "2020/day05-input.txt")
       (map find-seat)
       (group-by :row)
       (sort)
       (drop 1)
       (drop-last 1)
       (some (fn [[row seats]]
               (some->> (into #{} (map :col seats))
                        (set/difference (set (range 0 8)))
                        (first)
                        (hash-map :row row :col)
                        (seat->id))))))
