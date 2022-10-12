(ns aoc.day3
  (:require [aoc.utils :as u]
            [clojure.string :as s]))


(defn run-1 []
  (let [in (u/get-day-data 3 seq :test)
        rows (map (fn [l] (cycle l)) in)]
    (->> (map-indexed 
            (fn [i r] (nth r (* i 3)))
            rows) 
         (filter #(= % \#))
         count)))



(defn traverse-fn [right down in-data]
    (let [rows (map (fn [n l] [n (cycle l)]) (cons 0 (cycle (range 1 (inc down))))  in-data)
          rows (filter (fn [[n l]] (= n down)) rows)     
          rows (map second rows)]
      (->> (map-indexed 
              (fn [i r]  (nth r (* (inc i) right)))
              rows) 
           (filter #(= % \#))
           count)))
      ;(map (fn [e] (apply str (take 60 e))) rows)))
        
(defn run-1-1 []
  (let [in-data (u/get-day-data 3 seq)]
    (apply *
      [(traverse-fn 1 1 in-data)     
       (traverse-fn 3 1 in-data)     
       (traverse-fn 5 1 in-data)     
       (traverse-fn 7 1 in-data)     
       (traverse-fn 1 2 in-data)])))     
     
(defn run-1-2 []
  (let [in-data (u/get-day-data 3 seq :test)]
       (traverse-fn 1 2 in-data)))




 

