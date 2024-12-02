(ns aoc.day2
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn line-fn [s]
  (let [nums (s/split s #" +")]
    (mapv (fn [n] (Integer. n)) nums)))

(defn get-data []
  (u/get-day-data 2 line-fn))

(defn invalid-diff? [n]
  (or (< n 1) (> n 3)))

(defn inspect-report [nums]
  (if (> (nth nums 0) (nth nums 1))
    (->> (map - nums (drop 1 nums))
         (mapcat (fn [i n] (if (invalid-diff? n) [i] [])) (range 1 10)))
    (->> (map - (drop 1 nums) nums)
         (mapcat (fn [i n] (if (invalid-diff? n) [i] [])) (range 1 10)))))


(defn drop-nth [coll n]
  (concat
    (take n coll)
    (drop (inc n) coll)))


(defn valid-report-2? [rep]
  (or
    (empty? (inspect-report rep))
    (some empty?
      (map inspect-report
        (for [i (range 0 (count rep))]
          (drop-nth rep i))))))


(defn part-1 []
  (let [data (get-data)]
    (->> data
         (map inspect-report)
         (filter empty?)
         count)))

(defn part-2 []
  (let [data (get-data)]
    (->> data
         (filter valid-report-2?)
         count)))

