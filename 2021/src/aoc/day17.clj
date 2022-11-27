(ns aoc.day17
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as cset]))

;target area: x=60..94, y=-171..-136
(def input-data
  (let [in (slurp "./resources/day-17.txt")
        ;in "target area: x=20..30, y=-10..-5" ;test
        [from-x to-x from-y to-y] (map read-string (rest (first (re-seq #"target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+).*" in))))]
    {:from-x from-x :to-x to-x :from-y from-y :to-y to-y})) 


(def min-x (:from-x input-data))
(def min-y (:from-y input-data)) 
(def max-x (:to-x input-data)) 
(def max-y (:to-y input-data))

(defn is-in-target? [x y]
  (and (>= x min-x)
       (<= x max-x)
       (>= y min-y)
       (<= y max-y)))

(defn is-in-target-x [x]
  (and (>= x min-x)
       (<= x min-y)))

(defn iabs [x]
  (if (< x 0) (* x -1) x))


(defn dist-x [x]
  (-> x
      (* (inc x))
      (/ 2)
      inc))

(defn find-min-valid-x []
  (loop [x 1] 
    (if (< (dist-x x) min-x)
      (recur (inc x))
      x)))

(defn find-max-valid-x [min-valid-x]
  (loop [x min-valid-x] 
      (if (> (dist-x x) max-x)
        x
        (recur (inc x)))))


(defn run-1 []
  (if (> min-y 0) 
    (throw (Exception. "Algorithm in this code is was not considered for positive y values"))
    (println "run-1 answer: " (quot (* min-y (inc min-y)) 2))))


(defn calc-target-pos [start-x start-y end-x end-y]
  (loop [x start-x y start-y xa 0 ya 0]
    (let [xa (+ xa x) ya (+ ya y)]
      (cond
        (is-in-target? xa ya) [start-x start-y]
        (or (> x end-x) (< y end-y)) nil
        :default (recur (if (zero? x) x (dec x)) (dec y) xa ya)))))

(defn gen-positions []
  (let [start-x (find-min-valid-x)
        start-y min-y
        end-x   max-x
        end-y   (inc (* -1 min-y))]
    (for [x (range start-x (inc end-x))
          y (range start-y (inc end-y))]
      (calc-target-pos x y max-x min-y)))) 

(defn run-2 []
  (->> (gen-positions)
       (filter (comp not nil?)) 
       count))
       
     
  
