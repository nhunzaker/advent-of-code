(ns advent-of-code.2020.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (io/resource "2020/day03-input.txt"))

(defn forest [file]
  (->> (io/reader file)
       (slurp)
       (str/split-lines)
       (map cycle)))

(def tree \#)

(defn print-row [line char pos-right]
  (printf "%s%s\n"
          (str/join (take pos-right line))
          char))

(defn snow-sledding [move-right move-down]
  (let [lines (forest input)
        end (count lines)]
    (loop [hits 0
           right 0
           down 0]
      (if (<= end down)
        hits
        (let [line (nth lines down)
              char (nth line right)
              hit? (= tree char)]
          (printf "%3d (%s) | " down char)
          (print-row line (if hit? "X" "O") right)
          (recur (if hit? (inc hits) hits)
                 (+ right move-right)
                 (+ down move-down)))))))

(defn answer-1 []
  (snow-sledding 3 1))

(defn answer-2 []
  (* (snow-sledding 1 1)
     (snow-sledding 3 1)
     (snow-sledding 5 1)
     (snow-sledding 7 1)
     (snow-sledding 1 2)))
