(ns aoc.day14
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn line-fn [[y _ round-stones square-stones] s]
  (let [[ex rsa ssa] (reduce (fn [[x rs ss] c]
                              (cond
                                (= c \#) [(inc x) rs (conj ss [y x])] 
                                (= c \O) [(inc x) (conj rs [y x]) ss] 
                                :else    [(inc x) rs ss]))
                            [0 round-stones square-stones] s)]
    [(inc y) ex rsa ssa]))


(defn get-data [& args] 
  (u/get-day-data-reduce 14 line-fn [0 0 #{} #{}] (first args))) 

(defn ppp [t x]
  (println t x)
  x)



(defn print-col [max-y x round-stones square-stones]
  (println (for [y (range max-y)] (cond (contains? round-stones [y x]) "O" (contains? square-stones [y x]) "#" :else "."))))

(defn find-free [max-y x y rs ss]
  (loop [y y]
    (cond
      (>= y max-y)
      max-y

      (or (contains? rs [y x]) (contains? ss [y x]))
      (recur (inc y))

      :else
      y)))

(defn calculate-col-north [max-y x round-stones square-stones]
  (loop [y 0 last-free-y (find-free max-y x 0 round-stones square-stones) rs round-stones]
    ;(when (#{9} x) (println "y" y "last" last-free-y) (print-col max-y x rs square-stones))
    (if (>= y max-y)
      rs
      (cond (contains? square-stones [y x])
            (recur (inc y) (find-free max-y x y rs square-stones) rs)

            (contains? rs [y x])
            (if (> y last-free-y)
              (let [new-rs (conj (disj rs [y x]) [last-free-y x])]
                (recur (inc y) 
                       (find-free max-y x (inc last-free-y) new-rs square-stones)
                       new-rs))
              (recur (inc y) last-free-y rs)) 

            :else
            (recur (inc y) last-free-y rs)))))

(defn calculate-fall-north [max-y max-x round-stones square-stones]
  (reduce (fn [rs x] (calculate-col-north max-y x rs square-stones)) round-stones (range max-x))) 

(defn calculate-load [max-y rs]
  (reduce (fn [a [y _]] (+ a (- max-y y))) 0 rs)) 

(defn part-1 [& args]
  (let [[max-y max-x round-stones square-stones] (get-data (first args))
        round-stones (calculate-fall-north max-y max-x round-stones square-stones)] 
    (calculate-load max-y round-stones)))

(part-1 :test)

;(get-data :test)
