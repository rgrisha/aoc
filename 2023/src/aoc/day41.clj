(ns aoc.day41
  (:require
            [aoc.utils      :as u]
            [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as str]))



(defn line-wins [line]
  (->> (str/split line #"(:| \|) +")
       (drop 1)
       (map #(into #{} (map parse-long) (str/split % #" +")))
       (apply set/intersection)
       count))

  


(defn- line-points [line]
  (->> (line-wins line) dec (math/pow 2) int))

(defn pile-worth [lines]
  (->> lines (map line-points) (reduce +)))

(defn part1 [& args]
  (pile-worth (u/get-day-data 4 identity (first args))))

(defn cards-won [lines]
  (let [cards-wins (mapv line-wins lines)]
    (->> (range (dec (count cards-wins)) -1 -1)
         (reduce (fn [cards-adds idx]
                   (let [nidx (inc idx)]
                     (->> (range nidx (+ nidx (nth cards-wins idx)))
                          (map cards-adds)
                          (reduce + 1)
                          (assoc cards-adds idx))))
                 {})
         (vals)
         (reduce +))))

(defn part2 [& args]
  (cards-won (u/get-day-data 4 identity (first args))))
