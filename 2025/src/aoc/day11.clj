(ns aoc.day11
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn line-fn [s]
  (let [ss (s/split s #"[: ]")]
    [(first ss) (into [] (drop 2 ss))]))

(defn get-data [t]
  (into {}
    (u/get-day-data 11 line-fn t)))


(defn calc-to-out [data]
  (loop [layer {"you" 1}]
    (let [next-layer (mapcat
                       (fn [[k v]] (if (= k "out")
                                     [[k v]]
                                     (map (fn [nx] [nx v]) (get data k))))
                       layer)
          non-outs (filter (fn [[k _]] (not= k "out")) next-layer)
          next-layer (group-by first next-layer)
          next-layer (reduce-kv (fn [m k v] (assoc m k (apply + (map second v)))) {} next-layer)]
      (if (seq non-outs)
        (recur next-layer)
        next-layer))))



(defn part-1-pm [t]
  (let [net (get-data t)
        ca (calc-to-out net)]
    (get ca "out")))

(defn part-1 []
  (part-1-pm :nil))
