(ns aoc.day1
  (:require [aoc.utils :as u]))

(def data (u/get-day-data 1 (fn [v] (Long/parseLong v))))

(defn find-pair [search-num data]
  (let [pairs (into #{} data)
        find-fn (fn [v] (contains? pairs (- search-num v))) 
        answer-fn (fn [v] [v (- search-num v) (* v (- search-num v))])]
    (some-> (filter find-fn data)
            first
            answer-fn)))


(defn run-1 []
  (let [data (u/get-day-data 1 (fn [v] (Long/parseLong v)))]
    (find-pair 2020 data)))

(defn run-2 []
  (let [data (u/get-day-data 1 (fn [v] (Long/parseLong v)))
        find-fn (fn [v] (when-let [a (find-pair (- 2020 v) data)] [v a]))]
    (some find-fn data))) 

