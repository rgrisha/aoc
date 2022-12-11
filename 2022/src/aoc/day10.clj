(ns aoc.day10
  (:require [aoc.utils :as u]
            [clojure.string :as s]))


(defn get-data [& ds]
  (u/get-day-data 10 (fn [line]
                       (let [ss (s/split line #" ")
                             op (keyword (first ss))]
                         (if (= op :addx)
                           [1 op (Integer/parseInt (second ss))]
                           [0 op])))
                  (first ds)))


(defn get-xs [data acc-fn]
  (loop [data data x 1 clock 1 acc []]
    (if-not data
      acc
      (let [[c op n] (first data)
            nacc (acc-fn acc clock x)]
        ;(when (contains? clocks clock)
        ;  (println "cc" clock (first data))
        (if (> c 0)
          (recur (cons [0 op n] (next data)) x (inc clock) nacc)
          (recur (next data) (if (= op :addx) (+ x n) x) (inc clock) nacc))))))

(defn run-1 [& ds]
  (let [data (get-data (first ds))
        xd (get-xs data (fn [acc clock x]
                          (if (contains? #{20 60 100 140 180 220} clock) (conj acc [x clock]) acc)))] 
    (reduce (fn [a [b c]] (+ a (* b c))) 0 xd))) 

(comment 
  (run-1))

(defn upd-2 [acc clock x]
  (let [clock (dec clock)
        row-pos (quot clock 40)
        row (get acc row-pos [])
        col (rem clock 40)
        pixel-val (if (and (>= col (dec x)) (<= col (inc x))) "#" ".")
        nrow (assoc row col pixel-val)]
    (assoc acc row-pos nrow)))
    

(defn run-2 [& ds]
  (let [data (get-data (first ds))
        xd (get-xs data upd-2)] 
    (doseq [x xd]
      (println (apply str x)))))

