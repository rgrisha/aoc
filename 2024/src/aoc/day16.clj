(ns aoc.day16
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))


(defn to-coords [area]
  (reduce-kv (fn [m y line]
               (reduce-kv (fn [[s e mm] x v]
                            (case v
                              \# [s e (conj mm [x y])]
                              \S [[x y] e mm]
                              \E [s [x y] mm]
                              [s e mm]))
                          m
                          (into [] line)))
             [nil nil #{}]
             (into [] area)))

(defn get-data [t]
  (let [area (u/get-day-data 16 identity t)
        [start end area] (to-coords area)]
    [start end area]))

