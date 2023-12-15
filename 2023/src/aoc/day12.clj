(ns aoc.day12
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn parse-line [l]
  (let [[l r] (s/split l #" +")]
    [l (mapv parse-long (s/split r #","))]))
 
(defn get-data [& params] 
  (u/get-day-data 12 parse-line (first params)))

(get-data :test)
