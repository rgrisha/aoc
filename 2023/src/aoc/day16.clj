(ns aoc.day16
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))


(defn line-fn [s]
  (into [] s))

(defn get-data [& args] 
  (u/get-day-data 16 line-fn (first args))) 

;(get-data :test)

(defn go-r [[y x _]] [y (inc x) :r])

(defn go-l [[y x _]] [y (dec x) :l])

(defn go-u [[y x _]] [(dec y) x :u])

(defn go-d [[y x _]] [(inc y) x :d])


(defn calc-ray [rmap [y x d :as p]]
  (let [cell ((rmap y) x)]
    (case cell
      \- 
      (case d
        :r [(go-r p)]
        :l [(go-l p)]
        :u [(go-l p) (go-r p)]
        :d [(go-l p) (go-r p)])

      \| 
      (case d
        :u [(go-u p)]
        :d [(go-d p)]
        :l [(go-d p) (go-u p)]
        :r [(go-d p) (go-u p)])

      \\ 
      (case d
        :u [(go-l p)]
        :d [(go-r p)]
        :l [(go-u p)]
        :r [(go-d p)])

      \/ 
      (case d
        :u [(go-r p)]
        :d [(go-l p)]
        :l [(go-d p)]
        :r [(go-u p)])

      \.
      (case d
        :u [(go-u p)]
        :d [(go-d p)]
        :l [(go-l p)]
        :r [(go-r p)]))))


(defn calc-rays [rmap rays]
  (mapcat (fn [r] (calc-ray rmap r)) rays))

(defn ray-affection-fn [rmap maxy maxx starting-ray]
  (loop [rays [starting-ray] visited-tiles #{starting-ray}]
    (if (seq rays)
      (let [next-rays (calc-rays rmap rays)
            next-rays (filter (fn [[y x _]] (and (>= x 0) (>= y 0) (< x maxx) (< y maxy))) next-rays)
            next-rays (remove visited-tiles next-rays)]
        (recur next-rays (into visited-tiles next-rays)))
      (count (into #{} (map (fn [[y x _]] [y x]) visited-tiles))))))

(defn part-1 [& args]
  (let [rmap (get-data (first args))
        maxy (count rmap)
        maxx (count (first rmap))]
    (ray-affection-fn rmap maxy maxx [0 0 :r])))

(defn gen-starting-pos [maxy maxx]
  (let [last-y (dec maxy)
        last-x (dec maxx)]
    (concat (map (fn [y] [y 0 :r])      (range maxy))
            (map (fn [y] [y last-x :l]) (range maxy))
            (map (fn [x] [0 x :d])      (range maxx))
            (map (fn [x] [last-y x :u]) (range maxx)))))


;8434 too low
(defn part-2 [& args]
  (let [rmap (get-data (first args))
        maxy (count rmap)
        maxx (count (first rmap))
        ray-starting-positions (gen-starting-pos maxy maxx)]
    (reduce max (map (fn [sp] (ray-affection-fn rmap maxy maxx sp)) ray-starting-positions))))

