(ns aoc.day9
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as cset]))

(defn parse-line [l]
  (->> l
       seq
       (mapv #(Integer. (str %1)))))

(defn get-neighbours [field y x]
  (filter (comp not nil?)
          [(get-in field [y x])
           (get-in field [(dec y) x])
           (get-in field [(inc y) x]) 
           (get-in field [y (dec x)])
           (get-in field [y (inc x)])])) 

(defn get-neighbours-2 [field y x]
  (filter first 
          [[(get-in field [y       x]) y       x]
           [(get-in field [(dec y) x]) (dec y) x]
           [(get-in field [(inc y) x]) (inc y) x] 
           [(get-in field [y (dec x)]) y       (dec x)]
           [(get-in field [y (inc x)]) y       (inc x)]]))

(defn get-min-neighbours [field y x]
  (let [nbr (get-neighbours-2 field y x)]
    (when (< (ffirst nbr) (apply min (map first (rest nbr))))
      (first nbr))))

(defn run-1 []
  (let [data  (u/get-day-data 9 parse-line)
        data (vec data)
        pr (fn [t d] (println t d) d)]
    (->> (for [y (range (count data)) x (range (count (get data y)))] (get-neighbours data y x)) 
         (filter (fn [[ p & xs]] (< p (apply min xs))))
         (map first)
         (map inc)
         (reduce +))))

(defn find-basin [field s [v y x]]
  (let [nbrs (get-neighbours-2 field y x)
        ;_ (println "nbrs: " nbrs)
        good-nbrs (filter #(and (> (first %1) v) (< (first %1) 9) (not (contains? s %1))) nbrs)
        ;_ (println "goods" good-nbrs)
        nbrs-added (into s good-nbrs)] 
    (if (empty? good-nbrs)
      nbrs-added      
      (into nbrs-added (apply concat (map (partial find-basin field nbrs-added) good-nbrs))) )))

(defn run-2 []
  (let [data  (u/get-day-data 9 parse-line)
        data (vec data)]
    (->> (for [y (range (count data)) x (range (count (get data y)))] (get-min-neighbours data y x)) 
         (filter (comp not nil?))
         (map #(find-basin data #{} %1))
         (map count)
         (map inc)
         (sort >)
         (take 3)
         (apply *)
         )))

;1474248 too high

(defn get-test-data []
   (let [data  (u/get-day-data 9 parse-line :test)]
     (vec data)))

