(ns advent-of-code.2020.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def src (io/resource "2020/day04-input.txt"))


;;; TOKENS


(defn scan [pattern left right content]
  (let [limit (count content)]
    (loop [right left]
      (if (>= right limit)
        (when (> right left)
          [left right])
        (let [value (subs content right (inc right))]
          (if (re-find pattern value)
            (recur (inc right))
            (when (> right left)
              [left right])))))))

(defn line-break-token [left right line]
  (when (empty? (str/trim line))
    [right right [{:type :next}]]))

(defn space-token [left right line]
  (when-let [[left right] (scan #" " left right line)]
    [left right nil]))

(defn prop-token [left right line]
  (when-let [[left right] (scan #"[^\s\n]" left right line)]
    [left right
     (let [[attr value] (str/split (subs line left right) #":")]
       [{:type :attr :value [attr value]}])]))

(defn some-token [left right content]
  (some #(% left right content)
        [line-break-token prop-token space-token]))

(defn tokenize [content]
  (let [limit (count content)]
    (loop [left 0
           right 0
           tokens []]
      (if (> right limit)
        tokens
        (if-let [[left right token] (some-token left right content)]
          (recur (inc right) (inc right) (concat tokens token))
          (recur left (inc right) tokens))))))

(defn tokenize-file [src]
  (->> (io/reader src)
       (line-seq)
       (mapcat tokenize)))


;;; PASSPORT PARSING


(defmulti parse-prop (fn [[key value]] key))

(defmethod parse-prop "byr" [[_ value]]
  (read-string value))

(defmethod parse-prop "iyr" [[_ value]]
  (read-string value))

(defmethod parse-prop "eyr" [[_ value]]
  (read-string value))

(defmethod parse-prop "hgt" [[_ value]]
  (if-let [[_ hgt unit] (re-find #"(\d+)(cm|in)" value)]
    [(read-string hgt) unit]
    [(read-string value) "unknown"]))

(defmethod parse-prop "hcl" [[_ value]]
  value)

(defmethod parse-prop "ecl" [[_ value]]
  value)

(defmethod parse-prop "pid" [[_ value]]
  value)

(defmethod parse-prop "cid" [[_ value]]
  value)

(defn parse-props [passport]
  (reduce (fn [passport prop]
            (assoc passport (key prop) (parse-prop prop)))
          {}
          passport))

(defn materialize [props]
  (->> (into {} (map :value) props)
       (parse-props)))

(defn passports [src]
  (->> (tokenize-file src)
       (partition-by :type)
       (remove (comp #{:next} :type first))
       (map materialize)))


;;; VALIDATION


;; Not required: "cid"
(def required #{"byr" ;; birth year
                "iyr" ;; issue year
                "eyr" ;; expiration year
                "hgt" ;; height
                "hcl" ;; hair color
                "ecl" ;; eye color
                "pid" ;; passport id
                })

(defn valid-keys? [passport]
  (set/subset? required (set (keys passport))))

(defn between? [n min max]
  (and (>= n min) (<= n max)))

(defn valid-byr? [passport]
  (some-> (get passport "byr")
          (between? 1920 2020)))

(defn valid-iyr? [passport]
  (some-> (get passport "iyr")
          (between? 2010 2020)))

(defn valid-eyr? [passport]
  (some-> (get passport "eyr")
          (between? 2020 2030)))

(defn valid-hgt? [passport]
  (let [[hgt unit] (get passport "hgt")]
    (case unit
      "cm" (between? hgt 150 193)
      "in" (between? hgt 59 76)
      #_else false)))

(defn valid-hcl? [passport]
  (some->> (get passport "hcl")
           (re-matches #"#[0-9a-f]{6}")))

(def eye-colors #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})

(defn valid-ecl? [passport]
  (some->> (get passport "ecl")
           (contains? eye-colors)))

(defn valid-pid? [passport]
  (some->> (get passport "pid")
           (re-matches #"[0-9]{9}")))

(defn valid-data? [passport]
  (and
    (valid-byr? passport)
    (valid-iyr? passport)
    (valid-eyr? passport)
    (valid-hgt? passport)
    (valid-hcl? passport)
    (valid-ecl? passport)
    (valid-pid? passport)))


;;; SOLUTION


(defn complete-passports [file]
  (->> (passports file)
       (filter valid-keys?)))

(defn answer-1 [file]
  (->> (complete-passports file)
       (count)))

(defn answer-2 []
  (->> (passports src)
       (filter valid-data?)
       (count)))
