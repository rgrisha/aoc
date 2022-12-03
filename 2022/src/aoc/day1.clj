(ns aoc.day1
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn line-fn [[a t] s]
  (if (s/blank? s)
    [(conj a t) []]
    [a (conj t (Integer/parseInt s))])) 
        

(defn get-data [] 
  (let [[a t] (u/get-day-data-reduce 1 line-fn [[] []])]
    (conj a t)))

(defn run-1 []
  (apply max  (map #(reduce + %) (get-data))))

(defn run-2 []
  (->> (get-data) (map #(reduce + %))
       (sort (fn [a b] (- b a)))
       (take 3)
       (reduce +)))

