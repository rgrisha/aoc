(ns aoc.day5
  (:require [clojure.string :as s]
            [aoc.utils      :as u]
            [clojure.set :as cs]))

(defn line-fn [l]
  (map-indexed (fn [i e] [i e]) (seq l)))

(defn get-data [t]
  (let [lines (u/get-day-data 4 line-fn t)
