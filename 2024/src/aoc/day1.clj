(ns aoc.day1
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn line-fn [s]
  (let [[l r] (s/split s #" +")]
    [(Integer. l) (Integer. r)]))

(defn get-data []
  (u/get-day-data 1 line-fn))

(defn part-1 []
  (let [data (get-data)
        l (mapv first data)
        r (mapv second data)
        l (sort l)
        r (sort r)
        s (map (fn [a b] (abs (- a b))) l r)]
    (reduce + s)))

(defn part-2 []
  (let [data (get-data)
        l (mapv first data)
        r (mapv second data)
        fr (frequencies r)]
    (->> l
         (map (fn [n] (* n (get fr n 0))))
         (reduce +))))

