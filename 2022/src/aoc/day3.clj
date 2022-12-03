(ns aoc.day3
  (:require [clojure.string :as s]
            [aoc.utils      :as u]
            [clojure.set    :as cset]))
        

(defn get-data [] 
  (let [v (u/get-day-data 3 (fn [s] (split-at (quot (count s ) 2) s)))]
    v))

(defn priority [a]
  (let [a (int a)]
    (if (> a 96) (- a 96) (- a 38))))

(defn run-1 []
  (->> (get-data)  
       (map #(cset/intersection (into #{} (first %)) (into #{} (second %))))
       (map #(map priority %))
       (map #(reduce + %))
       (reduce +)))
       
(comment
  (first
    (run-2)))


(defn run-2 []
  (->> (get-data)
       (map #(into #{} (concat (first %) (second %))))
       (partition 3)
       (map #(apply cset/intersection %))
       (map #(map priority %))
       (map #(reduce + %))
       (reduce +)))

