(ns advent-of-code.2020.day04-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [advent-of-code.2020.day04 :as day04]))

(deftest test-valid-passport?
  (let [valid-passport {"byr" "2005"
                        "iyr" "2020"
                        "eyr" "2021"
                        "hgt" "157cm"
                        "hcl" "z"
                        "ecl" "lzr"
                        "pid" "152cm"
                        "cid" "254"}]
    ;; Required:
    (is (= false (day04/valid-passport? (dissoc valid-passport "byr"))))
    (is (= false (day04/valid-passport? (dissoc valid-passport "iyr"))))
    (is (= false (day04/valid-passport? (dissoc valid-passport "eyr"))))
    (is (= false (day04/valid-passport? (dissoc valid-passport "hgt"))))
    (is (= false (day04/valid-passport? (dissoc valid-passport "hcl"))))
    (is (= false (day04/valid-passport? (dissoc valid-passport "ecl"))))
    (is (= false (day04/valid-passport? (dissoc valid-passport "pid"))))
    ;; Optional:
    (is (= true (day04/valid-passport? (dissoc valid-passport "cid"))))
    ;; Extra fields:
    (is (= true (day04/valid-passport? (assoc valid-passport "whatever" true))))))

(deftest answer-1
  (testing "question sample"
    (is (= 2 (count (day04/complete-passports (io/resource "2020/day04-sample.txt"))))))

  (testing "puzzle data"
    (doseq [passport (day04/complete-passports (io/resource "2020/day04-input.txt"))]
      (is (contains? passport "byr"))
      (is (contains? passport "iyr"))
      (is (contains? passport "eyr"))
      (is (contains? passport "hgt"))
      (is (contains? passport "hcl"))
      (is (contains? passport "ecl"))
      (is (contains? passport "pid")))))
