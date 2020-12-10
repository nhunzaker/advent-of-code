(ns advent-of-code.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn get-content [resource]
  (->> (io/resource resource)
       (slurp)))

(defn get-lines [resource]
  (->> (io/resource resource)
       (io/reader)
       (line-seq)))

(defn line-clusters [resource]
  (-> (get-content resource)
      (str/split #"\n\n")))

(defn sum [data]
  (reduce + 0 data))

(defn sum-by [f data]
  (transduce (map f) + 0 data))

(defn pairs [list]
  (when-some [[left & remainder] list]
    (lazy-cat (for [right remainder] [left right])
              (pairs remainder))))
