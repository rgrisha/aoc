(ns aoc.day11
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn parse-line [l]
  (mapv identity l))
 
(defn get-data [ & params] 
  (u/get-day-data 11 parse-line (first params)))


(defn empty-row-numbers [rows]
  (->> rows
       (keep-indexed (fn [i row] (when (every? #(= \. %) row) i)))
       (into #{})))

(defn empty-col-numbers [rows]
  (into #{}
    (keep-indexed
      (fn [i s] (when (= s #{\.}) i))
      (reduce 
        (fn [a v] (mapv (fn [ma me] (conj ma me)) a v))
        (mapv (fn [e] #{e}) (first rows))
        (rest rows)))))

(defn galaxy-positions [data]
  (reduce-kv  
    (fn [a y v] (concat a (keep-indexed (fn [x e] (when (= e \#) [y x])) v)))
    []
    data))

(defn galaxy-pairs-calc [galaxies]
  (loop [gs galaxies pairs []]
    (if (< (count gs) 2)
      pairs
      (recur (next gs) (concat pairs (map (fn [g] [(first gs) g]) (next gs)))))))



(defn calc-distance [i1 i2 empts exp-rate]
  (cond
    (< i1 i2) (+ (- i2 i1) (* (dec exp-rate) (count (filter (fn [e] (and (> e i1) (< e i2))) empts))))
    (< i2 i1) (+ (- i1 i2) (* (dec exp-rate) (count (filter (fn [e] (and (> e i2) (< e i1))) empts))))
    :else     0))

(defn calc-galaxy-distance [exp-rate empty-rows empty-cols [[y1 x1] [y2 x2]]]
  (+ (calc-distance y1 y2 empty-rows exp-rate)
     (calc-distance x1 x2 empty-cols exp-rate)))

(defn result [expansion-rate & args]
  (let [data (get-data (first args))
        empty-rows (empty-row-numbers data)
        empty-cols (empty-col-numbers data)
        galaxies   (galaxy-positions data)
        galaxy-pairs (galaxy-pairs-calc galaxies)
        dist-fn    (partial calc-galaxy-distance expansion-rate empty-rows empty-cols)
        distances  (map dist-fn galaxy-pairs)]
    (reduce + 0 distances)))

(defn part-1 [& args]
  (result 2 (first args)))

(defn part-2 [& args]
  (result 1000000 (first args)))

