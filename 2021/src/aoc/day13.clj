(ns aoc.day13
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as cset]))

(defn fold [ufn points fold-line]
  (into #{} (map (partial ufn fold-line) points)))

(defn fold-y-point [fline [x y]]
  (if (> y fline)
    [x (- (* 2 fline) y)]
    [x y]))

(defn fold-x-point [fline [x y]]
  (if (> x fline)
    [(- (* 2 fline) x) y]
    [x y]))


(def fold-fns {"x" fold-x-point
               "y" fold-y-point})

(defn debug-print [points mx my]
  (doseq [y (range my)]
    (doseq [x (range mx)]
      (print (if (contains? points [x y]) "#" ".")))
    (println ""))) 

(defn run-1 []
  (let [data  (u/get-day-data 13 #(s/split %1 #",") )
        [points _ folds] (partition-by #(= "" (first %1)) data)
        points (map (fn [[a b]] [(Integer. a) (Integer. b)]) points)
        folds (map #(->> (re-matches #"fold along ([x,y])=([0-9]+)" (first %1)) (drop 1))  folds) 
        folds (map (fn [[a b]] [a (Integer. b)]) folds)
        [first-fold-fn first-fold-line] (first folds)
        ]
    (count (fold (fold-fns first-fold-fn) points first-fold-line))
    ))

(defn run-2 []
  (let [data  (u/get-day-data 13 #(s/split %1 #",") )
        [points _ folds] (partition-by #(= "" (first %1)) data)
        points (map (fn [[a b]] [(Integer. a) (Integer. b)]) points)
        folds (map #(->> (re-matches #"fold along ([x,y])=([0-9]+)" (first %1)) (drop 1))  folds) 
        folds (map (fn [[a b]] [a (Integer. b)]) folds)
        [first-fold-fn first-fold-line] (first folds)
        ]
    (debug-print (reduce (fn [a [vd vn]] (fold (fold-fns vd) a vn)) points folds) 80 10)
    ))
