(ns advent-of-code.2020.day01-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [advent-of-code.2020.day01 :as day01]))

(def number-pool (day01/read-numbers day01/input))

(deftest find-match
  (doseq [n [2 3]]
    (let [solution (day01/find-match 2020 n number-pool)]
      (is (= 2020 (reduce + 0 solution))
          "Should add up to 2020")

      (is (every? number-pool solution)
          "All numbers should exist in original data set"))))
