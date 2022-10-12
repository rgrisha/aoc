(ns aoc.day5
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as cset]))

(defn parse-line [l]
  (let [pairs (s/split l #" -> ") ]
    (map (fn [pair] (map (fn [p] (Integer. p)) (s/split pair #","))) pairs)))

(defn gen-line-seq [a b]
  (if (< a b)
    (range a (inc b))
    (reverse (range b (inc a)))))

(defn gen-squared-line [[[from-x from-y] [to-x to-y]]]
  (cond (= from-x to-x) (map (fn [y] [from-x y]) (gen-line-seq from-y to-y)) 
        (= from-y to-y) (map (fn [x] [x from-y]) (gen-line-seq from-x to-x))))

(defn gen-line [[[from-x from-y] [to-x to-y]]]
  (cond (= from-x to-x) (map (fn [y] [from-x y]) (gen-line-seq from-y to-y)) 
        (= from-y to-y) (map (fn [x] [x from-y]) (gen-line-seq from-x to-x))
        :else (map (fn [x y] [x y]) (gen-line-seq from-x to-x) (gen-line-seq from-y to-y)))) 

(defn lines-to-area [line-fn ls]
  (let [points (->> ls 
                    (map line-fn)
                    (filter (comp not nil?)))
        all-points (apply concat points)]
    all-points))

(defn get-crossing-points [points]
  (let [groups (group-by identity points)]
    groups))

(defn gen-answer [line-fn]
  (let [data (u/get-day-data 5 parse-line) ]
    (->> data
         (lines-to-area line-fn)
         get-crossing-points 
         vals
         (filter (fn [v] (> (count v) 1)))
         count)))

(defn run-1 []
  (gen-answer gen-squared-line))

(defn run-2 []
  (gen-answer gen-line))
