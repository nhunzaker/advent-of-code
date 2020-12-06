(ns advent-of-code.2020.day06
  (:require [advent-of-code.utils :refer :all]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn consensus [reducer lines]
  (->> lines
       (eduction (map str/split-lines)
                 (map #(map set %))
                 (map #(reduce reducer %)))))

(defn answer-1 []
  (->> (line-clusters "2020/day06-input.txt")
       (consensus set/union)
       (sum-by count)))

(defn answer-2 []
  (->> (line-clusters "2020/day06-input.txt")
       (consensus set/intersection)
       (sum-by count)))
