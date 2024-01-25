(ns aoc.day21
  (:require [clojure.set    :as cs]
            [aoc.utils      :as u]))


(defn get-data [& args] 
  (let [lines (u/get-day-data 21 identity (first args))]
    (reduce-kv (fn [m y l]
                 (reduce-kv (fn [ml x c] (update ml c #(conj % [y x]))) m (into [] l)))
               {\# #{} \S #{} \. #{}} 
               lines))) 

(defn gen-moves-for-coord [plots]
  (fn moves-for-coord [[y x]]
    (keep plots [[(dec y) x] [(inc y) x] [y (dec x)] [y (inc x)]])))
   
(defn calc-steps [plots initial iterations]
  (let [moves-fn (gen-moves-for-coord plots)]
    (loop [i iterations walked #{initial}]
      (if (zero? i)
        walked
        (let [next-coords (into #{} (mapcat moves-fn walked))]
          (recur (dec i)  (cs/difference next-coords walked)))))))
  

(defn part-1 [& args]
  (let [maps (get-data (first args))
        start (first (get maps \S))
        plots (conj (get maps \.) start)
        steps (calc-steps plots start 64)]
    (count steps)))


