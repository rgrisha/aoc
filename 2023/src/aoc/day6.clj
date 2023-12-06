(ns aoc.day6
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))
 
(defn parse-data [l]
  (drop 1 (map parse-long (s/split l #" +"))))
  
(defn get-data [ & params] 
   (let [dd (u/get-day-data 6 parse-data (first params))]
     (map (fn [a b] [a b]) (first dd) (second dd)))) 

;(get-data :test)

(defn travel-distances [time-max]
  (->> (range 1 time-max)
       (map (fn [t] (* (- time-max t) t)))))

(defn ways-to-win [[time dist]]
  (->> (travel-distances time)
       (filter #(> % dist)) 
       count)) 

(defn part-1 [ & params]
  (->> (get-data (first params))  
       (map ways-to-win)
       (apply *))) 
      
(defn part-2 []
  (ways-to-win [ 62737565 644102312401023]))
