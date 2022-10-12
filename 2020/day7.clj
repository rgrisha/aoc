(ns aoc.day7
  (:require [aoc.utils :as u]
            [clojure.string :as s]))

(defn bag-norm [b]
  (let [bag (s/trim b)
        bagp (re-matches #"(\d+) ([a-z ]+) bag.*" bag)]
    (if (nil? bagp) 
      nil 
      [(nth bagp 2) (Integer. (nth bagp 1))])) )

(defn parse-text [a s]
  (let [[bag bags] (s/split s #" bag[s]* contain " ) 
        bags (s/split bags #",")
        bags (map bag-norm bags)] 
    (assoc a bag bags )))


(defn bag-chain [d n]
  (->> (get d n)
       (tree-seq  
         (fn  [a] (not (nil? (first a) )))
         (fn  [k] (map (fn [v] (get d (first v)) ) k)))
       (apply concat)
       (filter (comp not nil?))
       (map first)
       (into #{})))

(defn answer-1 [d n]
  (->> (keys d)
       (filter (comp (fn [z] (contains? z n)) (partial bag-chain d)) ) 
       (into #{})
       count
       ))


(defn run-1 []
  (let [in (u/get-day-data-reduce 7 parse-text {})]
    (answer-1 in "shiny gold") ))


(defn count-2 [data bag]
  (let [sub-bags (get data bag)] 
    (if (nil? (first sub-bags))
        1
        (inc (apply + (map (fn [[b c]] (* c (count-2 data b))) sub-bags)
           )))))

(defn run-2 []
  (let [data (u/get-day-data-reduce 7 parse-text {})]
    (dec (count-2 data "shiny gold"))))

