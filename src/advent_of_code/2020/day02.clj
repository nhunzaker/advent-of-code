(ns advent-of-code.2020.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (io/resource "2020/day02-input.txt"))

(defn lines [file]
  (->> (io/reader file)
       (slurp)
       (str/split-lines)))

(def reg-parse #"(\d+)-(\d+) (\w): (.+)")

(defn parse [line]
  (when-let [[_ min max letter password] (re-matches reg-parse line)]
    [(read-string min) (read-string max) (first letter) password]))

;; -------------------------------------------------- ;;
;; Answer 1
;; -------------------------------------------------- ;;

(defn check-count [[min max letter password]]
  (let [total (count (filter #(= letter %) password))]
    (and (<= min total)
         (>= max total))))

(defn answer-1 []
  (->> (lines input)
       (map parse)
       (filter check-count)
       (count)
       (printf "%s passwords are valid\n")))


;; -------------------------------------------------- ;;
;; Answer 2
;; -------------------------------------------------- ;;

(defn check-index [[left right letter password]]
  (let [left-letter (get password (dec left))
        right-letter (get password (dec right))]
    (not= (= left-letter letter)
          (= right-letter letter))))

(defn answer-2 []
  (->> (lines input)
       (map parse)
       (filter check-index)
       (count)
       (printf "%s passwords are valid\n")))


;; -------------------------------------------------- ;;
;; Answer 1 Redux
;;
;; Silly state machine just for fun
;;
;;   min -> max -> letter -> password -> repeat
;;
;; -------------------------------------------------- ;;

(defmulti state
  "Behavior for the given state of the parser"
  (fn [ctx _char]
    (:state ctx)))

(defn run-machine! [str]
  (reduce state {:state :min :size 0 :valid 0} str))

;; Min State -----------------------------------------;;

(defn min->max [ctx]
  (-> (assoc ctx
             :state :max
             :min (read-string (:min-swap ctx)))
      (dissoc :min-swap)))

(defmethod state :min [ctx char]
  (case char
    \- (min->max ctx)
    (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9) (update ctx :min-swap str char)))

;; Max State -----------------------------------------;;

(defn max->letter [ctx]
  (-> (assoc ctx
             :state :letter
             :max (read-string (:max-swap ctx)))
      (dissoc :max-swap)))

(defmethod state :max [ctx char]
  (case char
    \space (max->letter ctx)
    (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9) (update ctx :max-swap str char)))

;; Letter State --------------------------------------;;

(defn letter->password [ctx]
  (assoc ctx :state :password))

(defmethod state :letter [ctx char]
  (case char
    \: (letter->password ctx)
    \space ctx
    #_else (assoc ctx :letter char)))

;; Password State ------------------------------------;;

(defn within-range? [{:keys [min max size] :as ctx}]
  (and (<= min size)
       (>= max size)))

(defn password->min [ctx]
  (-> (cond-> ctx
        (within-range? ctx) (update :valid inc))
      (assoc :state :min :size 0)
      (dissoc :min :max :letter)))

(defmethod state :password [ctx char]
  (condp = char
    \newline (password->min ctx)
    (:letter ctx) (update ctx :size inc)
    #_else ctx))

;; Result --------------------------------------------;;

(defn answer-3 []
  (->> (run-machine! (str (slurp input)))
       :valid
       (printf "%s passwords are valid\n")))
