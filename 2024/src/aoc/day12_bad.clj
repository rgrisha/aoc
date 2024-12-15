(ns aoc.day12
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn line-fn [s]
  (mapv identity s))

(defn get-data [t]
  (u/get-day-data 12 line-fn t))


(defn find-neighbour [[x y c] regions]
  (let [find-reg-index (fn [coord]
                         (some (fn [[r i]] (when (contains? r coord) i)) 
                               (map (fn [a b] [a b]) regions (range))))])
                         
  (or
    (some (fn [[r i]] (when (contains? r [(dec x) y c]) i)) 
          (map (fn [a b] [a b]) regions (range)))
    (some (fn [[r i]] (when (contains? r [x (dec y) c]) i)) 
          (map (fn [a b] [a b]) regions (range)))))


(defn find-regions-per-line [y data regions]
  (let [line (get data y)]
    (loop [xs (range 0 (count (first data))) regs regions]
      (let [x (first xs)]
        (if x
          (let [c (get line x)
                nhi (find-neighbour [x y c] regs)]
            (if nhi
              (recur (next xs)
                     (update regs nhi (fn [r] (conj r [x y c]))))
              (recur (next xs)
                     (conj regs #{[x y c]}))))
          
          regs)))))

(cluster-regions (get-data :test))
(defn cluster-regions [data]
  (reduce (fn [regions y] 
            (find-regions-per-line y data regions))
          []
          (range 0 (count data))))

(defn part-1 [& t]
  (let [data (get-data (first t))]))

