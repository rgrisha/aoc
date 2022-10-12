(ns aoc.day1
  (:require [aoc.utils :as u]))


(defn answer-1 [data]
  (->> (map (fn [a b] (if (> a b) 1 0)) (rest data) data)
       (reduce + )))


(defn run-1 []
  (let [data (u/get-day-data 1 (fn [v] (Integer. v)))]
    (answer-1 data)))

 
(defn gen-windows-sum [data]
  (map + data (rest data) (rest (rest data))))

(defn run-2 []
  (let [data (u/get-day-data 1 (fn [v] (Integer. v)))]
    (answer-1 (gen-windows-sum data))))

