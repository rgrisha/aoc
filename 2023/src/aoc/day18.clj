(ns aoc.day18
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))


(defn line-fn [s]
  (let [p (s/split s #" +|\(|\)|#")
        [d n c] (into [] (remove #(= % "") p))]
   [(keyword (s/lower-case d)) (parse-long n) c])) 

(line-fn "R 6 (#70c710)")

(defn get-data [& args] 
  (u/get-day-data 18 line-fn (first args))) 


(defn add-coords [coords y x n step]
  (println "add coords with " y x n step)
  (loop [coords coords i 0 y y]
    (if (< i n)
      (recur (update coords y (fn [c] (cons x c))) (inc i) (+ y step))
      coords)))


(defn part-1 [& args]
  (let [data (get-data (first args))]
    (loop [current [0 0] coords {0 []} instrs data prev-direction nil]
      (if-let [[ins & inss] instrs]
        (let [[d n _] ins 
              [x y] current]
          (case d
            :u
            (recur [x (- y n)] 
                   (add-coords coords (dec y) x n -1) 
                   inss
                   d)

            :d
            (recur [x (+ y n)] 
                   (add-coords coords (inc y) x n 1) 
                   inss 
                   d) 
          
            :l 
            (recur [(- x n) y] 
                   (if (= prev-direction :u)
                     (update coords y (fn [c] (conj (or c []) [(- x n) x 1])))
                     (update coords y (fn [c] (conj (or c []) [(- x n) x -1]))))
                   inss
                   d)
          
            :r 
            (recur [(+ x n) y] 
                   (if (= prev-direction :d)
                     (update coords y (fn [c] (conj (or c []) [x (+ x n) -1])))
                     (update coords y (fn [c] (conj (or c []) [x (+ x n) 1]))))
                   inss
                   d)))
          
        coords))))
