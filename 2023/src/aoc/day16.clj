(ns aoc.day16
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(def els {\- {:r :r :l :l :u [:l :r] :d [:l :r]}
          \\ {:d :r :l :u :u :l :r :d}
          \| {:u :u :d :d :l [:u :d] :r [:u :d]}
          \/ {:u :r :l :d :d :l :r :u}})

(defn line-fn [s]
  (mapv #(els %) s))

(defn get-data [& args] 
  (u/get-day-data 16 line-fn (first args))) 

(defn ppp [t x]
  (println t x)
  x)

;(get-data :test)

(defn pos-from-direction [y x d]
  (case d
        :u [(dec y) x]
        :d [(inc y) x]
        :l [y (dec x)]
        :r [y (inc x)]))

(defn next-ray-step [emap [[y x] dir]]
  (let [current ((emap y) x)] 
    (if (nil? current)
      [[(pos-from-direction y x dir) dir]]
      (let [next-dirs (current dir)]
        (if (keyword? next-dirs)
          [[(pos-from-direction y x next-dirs) next-dirs]]
          (mapv (fn [d] [(pos-from-direction y x d) d]) next-dirs))))))  

 ;8250 too high 
(defn part-1 [& args]
  (let [emap (get-data (first args))
        first-ray [[[0 0] :r]]
        maxy (count emap)
        maxx (count (first emap))]
    (loop [rays first-ray i 0 visited-tiles #{first-ray}]    
      ;(println "rays:" rays)
      ;(when (= 32 (.read *in*)) (throw (java.lang.Exception.  (str "stop" (count-tiles emap)))))
      (if (seq rays)
        (let [next-rays-steps (mapcat (fn [r] (next-ray-step emap r)) rays)
              next-rays-steps (filter (fn [[[y x] _]] (and (>= y 0) (>= x 0) (< x maxx) (< y maxy))) next-rays-steps)
              next-rays-steps (remove #(visited-tiles %) next-rays-steps)]
          (recur next-rays-steps (inc i) (into visited-tiles next-rays-steps)))

        (count (into #{} (map first visited-tiles)))))))  


