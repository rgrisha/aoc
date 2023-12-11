(ns aoc.day9
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn parse-line [l]
  (mapv parse-long (s/split l #" +")))
 
(defn get-data [ & params] 
  (u/get-day-data 9 parse-line (first params)))

(defn predict-right [vs]
  (if (every? zero? vs)
    (conj vs 0)
    (let [layer (mapv - (next vs) vs)]
      (conj vs (+ (last vs) (last (predict-right layer)))))))

(defn part-1 [& args]
  (->> (get-data (last args))
       (mapv predict-right)
       (mapv last) 
       (reduce + 0)))

(defn predict-left [vs]
  (if (every? zero? vs)
    (cons 0 vs)
    (let [layer (mapv - (next vs) vs)]
      (cons (- (first vs) (first (predict-left layer))) vs))))

(defn part-2 [& args]
  (->> (get-data (last args))
       (mapv predict-left)
       (mapv first) 
       (reduce + 0)))
