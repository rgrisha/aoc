(ns aoc.day4
  (:require [clojure.string :as s]
            [aoc.utils      :as u]
            [clojure.set    :as cset]))

(defn get-data [] 
  (let [v (u/get-day-data 4 (fn [s] 
                              (mapv (fn [v] 
                                      (mapv #(Integer/parseInt %) (s/split v #"-"))) 
                                    (s/split s #","))))] 
                          
    v))

(defn fully-contains? [[[la ua] [lb ub]]]
  (or (and (>= la lb) (<= ua ub))
      (and (>= lb la) (<= ub ua))))

(defn between [a b c]
  (and (<= a b) (<= b c)))

(defn overlap? [[[la ua] [lb ub]]]
  (or (between lb la ub) 
      (between lb ua ub)
      (between la lb ua)
      (between la ub ua)))

        
(defn run-1 []
  (->> (get-data)
       (filter fully-contains?)
       count))

(defn run-2 []
  (->> (get-data)
       (filter overlap?)
       count))
