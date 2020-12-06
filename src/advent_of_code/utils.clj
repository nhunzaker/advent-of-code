(ns advent-of-code.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn get-lines [resource]
  (->> (io/resource resource)
       (io/reader)
       (line-seq)))
