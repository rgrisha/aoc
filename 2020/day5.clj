(ns aoc.day5
  (:require [aoc.utils :as u]
            [clojure.string :as s]))


(defn seat-number [n]
 (reduce  
   (fn  [a v] (+ (bit-shift-left a 1) v)) 
   (map (fn [v] (get  {\B 1 \F 0 \R 1 \L 0} v )) 
        (seq n))))



(defn run-1 []
  (let [in (u/get-day-data 5 seq)]
    (apply max (map seat-number in))))

(defn run-2 []
  (let [in (u/get-day-data 5 seq)]
    (map seat-number in)))
