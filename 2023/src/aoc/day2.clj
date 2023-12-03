(ns aoc.day2
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn conv-take [s]
  (let [[l r] (s/split s #" ")]
    [(keyword r) (Integer. l)]))


(defn line-fn [s]
  (let [[l r] (s/split s #": *")
        game-num  (Integer. (second (s/split l #" ")))  
        game-sets-raw (s/split r #"; *")    
        game-sets (mapv (fn [g] (into {} (mapv conv-take (s/split (s/trim g) #", *")))) 
                        game-sets-raw)]
    [game-num game-sets])) 

(defn get-data [] 
  (u/get-day-data 2 line-fn)) 

(def part-1-initial
  {:red 12 :green 13 :blue 14})

(defn subtract [{r1 :red, g1 :green b1 :blue :or {r1 0 g1 0 b1 0}} 
                {r2 :red, g2 :green b2 :blue :or {r2 0 g2 0 b2 0}}]
  {:red (- r1 r2) :green (- g1 g2) :blue (- b1 b2)})
   
(defn negative? [{r1 :red, g1 :green b1 :blue :or {r1 0 g1 0 b1 0}}]
  (or (< r1 0) (< g1 0) (< b1 0)))

(defn positive? [{r1 :red, g1 :green b1 :blue :or {r1 0 g1 0 b1 0}}]
  (and (>= r1 0) (>= g1 0) (>= b1 0)))


(defn part-1 []
  (->> (get-data)
       (map (fn [[game-num takes]] [game-num (map (fn [t] (subtract part-1-initial t)) takes)]))
       (filter (fn [[_ takes]] (every? positive? takes)))
       (map first)
       (reduce + 0)))

(defn max-take  [{r1 :red, g1 :green b1 :blue :or {r1 0 g1 0 b1 0} :as arg1} 
                 {r2 :red, g2 :green b2 :blue :or {r2 0 g2 0 b2 0} :as arg2}]
  (println "maxing " arg1 arg2)
  {:red (max r1 r2) :green (max g1 g2) :blue (max b1 b2)})

(defn part-2 []
  (->> (get-data)
       (map (fn [[_ takes]] (reduce max-take {:red 0 :green 0 :blue 0} takes)))
       (map (fn [a] (* (:red a) (:green a) (:blue a))))
       (reduce + 0)))

(part-2) 
