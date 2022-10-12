(ns aoc.day18
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as cset]
            [clojure.edn :as edn]))


(defn get-data []
  (let [data (u/get-day-data 18 edn/read-string)
        data (vec data)]
    data))

(defn snail-add [a b] [a b])

(defn concat-vec [a b] (reduce conj a b))

(defn tree-red [branch-red-fn leaf-red-fn init-el snn]
  (let [f-dw (fn deepwalk [[a b] level]
                 (cond-> init-el
                   (number? a) (leaf-red-fn a level)
                   (coll? a)   (branch-red-fn (deepwalk a (inc level))) 
                   (number? b) (leaf-red-fn b level) 
                   (coll? b)   (branch-red-fn (deepwalk b (inc level)))))]
    (f-dw snn 1))) 

            

(defn disassemble [s]
  (tree-red concat-vec (fn [a n l] (conj a [l n])) [] s))
  
(defn get-explode-pair [s]
  (->> s
       (map-indexed (fn [i [a b]] [i a b]))
       (drop-while (fn [[ _ a _]] (< a 4)))
       (take 2)))


(defn walk [n [a b]]
  (let [aa (if (number? a) [(inc n) a] (walk n a))
        bb (if (number? b) [(inc n) b] (walk n b))]
    [aa bb]))

(defn walk2 [v [a b]]
  (let [aa (if (number? a)
            (do
              (vswap! v inc)
              [@v a]) 
            (walk2 v a))
        bb (if (number? b)
            (do
              (vswap! v inc)
              [@v b]) 
            (walk2 v b))]
    [aa bb]))

(defn add-n [n ])

(defn walk3 [v [a b]]
  (let [[n1 aa] (if (number? a)
            [(inc v) (str v "-" a)] 
            (walk3 v a))
        [n2 bb] (if (number? b)
            [(inc n1) (str n1 "-" b)] 
            (walk3 n1 b))]
    [n2 [aa bb]]))
