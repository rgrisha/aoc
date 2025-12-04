(ns aoc.day4
  (:require [clojure.string :as s]
            [aoc.utils      :as u]
            [clojure.set :as cs]))

(defn line-fn [l]
  (map-indexed (fn [i e] [i e]) (seq l)))

(defn get-data [t]
  (let [lines (u/get-day-data 4 line-fn t)
        lix (map-indexed (fn [y es] (map (fn [[x e]] [x y e]) es)) lines)]
    (->> lix
         (apply concat)
         (filter (fn [[x y e]] (= e \@)))
         (map (fn [[x y _]] [x y]))
         (into #{}))))

(defn get-adjacents [[x y]]
  [[(dec x) (dec y)] [(dec x) y] [(dec x) (inc y)] [x (dec y)] [x (inc y)] [(inc x) (dec y)] [(inc x) y] [(inc x) (inc y)]])

(defn count-item-adjacents [e data]
  (->> e
       get-adjacents
       (filter (fn [ae] (contains? data ae)))
       count))

(defn count-adjacents-1 [data]
  (map (fn [e] (count-item-adjacents e data)) data))

(defn get-item-adjacents [e data]
  [e (->> e
       get-adjacents
       (filter (fn [ae] (contains? data ae)))
       count)])


(defn part-1 []
  (let [data (get-data nil)]
     (->> data
          (count-adjacents-1)
          (filter (fn [i] (< i 4)))
          count)))

(defn part-2 []
  (let [data (get-data nil)]
    (loop [data data acc 0]
      (let [adjs (map (fn [e] (get-item-adjacents e data)) data)
            adjs (filter (fn [[e c]] (< c 4)) adjs)
            adjs (map first adjs)
            adjs (into #{} adjs)]

        (if (empty? adjs)
          acc
          (recur (cs/difference data adjs) (+ acc (count adjs))))))))
