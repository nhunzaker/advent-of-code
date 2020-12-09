(ns advent-of-code.2020.day07
  (:require [advent-of-code.utils :refer :all]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn parser [line]
  (re-seq #"(.+?) bags* contain (.+)\.\n*" line))

(defn bag-color [match]
  (second match))

(defn quantify [section]
  (->> (re-seq #"(\d+?) (.+?) bags*" section)
       (into #{} (map #(vector (nth % 2) (read-string (nth % 1)))))))

(defn bag-contents [match]
  (let [contents (nth match 2)]
    (when-not (= "no other bags" contents)
      (quantify contents))))

(defn contains-bag? [reference query-color wanted-color]
  (let [bag (get reference query-color)]
    (or (contains? bag wanted-color)
        (some #(contains-bag? reference % wanted-color)
              (keys bag)))))

(defn count-bags [query-color reference]
  (->> (get reference query-color)
       (reduce (fn [total [child-color size]]
                 (+ total size (* size (count-bags child-color reference))))
               0)))

;; TODO FIX ME
(defn answer-1 []
  (let [reference (->> (get-content "2020/day07-input.txt")
                       (parser)
                       (into {} (map #(vector (bag-color %) (bag-contents %)))))]
    (filter #(contains-bag? reference % "shiny gold")
            (keys reference))))

(defn answer-2 []
  (->> (get-content "2020/day07-input.txt")
       (parser)
       (into {} (map #(vector (bag-color %) (bag-contents %))))
       (count-bags "shiny gold")))
