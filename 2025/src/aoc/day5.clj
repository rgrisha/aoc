(ns aoc.day5
  (:require [clojure.string :as s]
            [aoc.utils      :as u]
            [clojure.set :as cs]))

(defn line-fn [l]
  l)

(defn get-data [t]
  (let [lines (u/get-day-data 5 line-fn t)]
    (loop [ranges [] ids [] lines lines]
      (if (nil? lines)
        [ranges ids]
        (let [[f s] (s/split (first lines) #"-")]
          (cond
            (or (nil? f) (.isBlank f))
            (recur ranges ids (next lines))

            (nil? s)
            (recur ranges (conj ids (Long/parseLong f)) (next lines))

            :else
            (recur (conj ranges [(Long/parseLong f) (Long/parseLong s)]) ids (next lines))))))))

(defn part-1 []
  (let [[ranges ids] (get-data nil)]
    (->> ids
         (keep (fn [e] (some (fn [[f s]] (and (>= e f) (<= e s))) ranges)))
         count)))

(defn descartes [ranges]
  (for [a ranges
        b ranges
        :when (not= a b)]
    [a b]))

(defn merge-all-ranges [ranges]
  (let [red-fn (fn [[a [cel cer]] [el er]]
                 (if (< cer el)
                   [(conj a [cel cer]) [el er]]
                   [a [cel (max cer er)]]))
        ranges (sort-by first ranges)
        [a l] (reduce red-fn [[] (first ranges)] (next ranges))]
    (conj a l)))


; 355555479253787
(defn part-2 []
  (let [ranges (first (get-data nil))]
     (->> ranges
          merge-all-ranges
          (map (fn [[a b]] (inc (- b a))))
          (apply +))))
