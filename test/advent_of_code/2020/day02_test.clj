(ns advent-of-code.2020.day02-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [advent-of-code.2020.day02 :as day02]))

(deftest test-parse
  (is (= [7 9 \l "vslmtglbc"]
         (day02/parse "7-9 l: vslmtglbc"))))

(deftest test-check-count
  (is (= true (day02/check-count [2 5 \c "aabbccc"]))
      "In range")

  (is (= false (day02/check-count [2 5 \d "aabbccc"]))
      "Missing letter")

  (is (= true (day02/check-count [0 5 \d "aabbccc"]))
      "Absent, but 0 min")

  (is (= false (day02/check-count [5 5 \a "abbccc"]))
      "Too few")

  (is (= false (day02/check-count [0 5 \a "aaaaaabbccc"]))
      "Too many"))

(deftest test-check-index
  (is (= true (day02/check-index [1 3 \a "abcde"]))
      "position 1 contains a and position 3 does not")

  (is (= false (day02/check-index [1 3 \b "cdefg"]))
      "neither position 1 nor position 3 contains b")

  (is (= false (day02/check-index [2 9 \c "ccccccccc"]))
      "both position 2 and position 9 contain c")

  (is (= true (day02/check-index [2 20 \c "ccccccccc"]))
      "handles out of upper range")

  (is (= true (day02/check-index [-1 2 \c "ccccccccc"]))
      "handles out of lower range")

  (is (= false (day02/check-index [-1 20 \c "ccccccccc"]))
      "false when both are out of range"))

(deftest min-state
  (is (= {:state :max :min 12} (reduce day02/state {:state :min} "12-"))))

(deftest max-state
  (is (= {:state :letter :max 12} (reduce day02/state {:state :max} "12 "))))

(deftest letter-state
  (is (= {:state :password :letter \f} (reduce day02/state {:state :letter} " f:"))))

(deftest state-machine
  (is (= {:state :min :size 0 :valid 1}
         (day02/state-machine "1-2 f: foobar\n"))))
