(ns aoc.day6
  (:require [clojure.string :as s]
            [aoc.utils      :as u]
            [clojure.set    :as cset]
            [clojure.java.io :as io]))

(defn get-data []
  (slurp (-> "day-6.txt" io/resource io/reader)))

(defn part-1 [data distinct-count]
  (reduce (fn [{cnt :count stack :stack} v] 
            (if (= distinct-count (count (into #{} stack)))
              (reduced (dec (+ distinct-count cnt)))
              {:count (inc cnt) :stack (concat (drop 1 stack) [v])}))
          {:count 1 :stack (take distinct-count data)} 
          (drop distinct-count data)))

(defn run-1 []
  (part-1 (seq (get-data)) 4))

(defn run-2 []
  (part-1 (seq (get-data)) 14))
