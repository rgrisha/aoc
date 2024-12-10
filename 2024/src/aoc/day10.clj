(ns aoc.day10
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn line-fn [s]
  (mapv (fn [c] (- (int c) (int \0))) s))

(defn get-data [t]
  (u/get-day-data 10 line-fn (first t)))

(defn find-trailheads [data]
  (for [y (range 0 (count data))
        x (range 0 (count (first data)))
        :when (= 0 (get (get data y) x))]
    [x y]))

(defn next? [n [x y] data]
  (try
    (when (= n (get (get data y) x))
      [x y])
    (catch Exception e nil)))

(defn find-paths [ps p n [x y] data]
  (if (= n 9)
    (conj ps (conj p [x y]))
    (let [nv (inc n)
          nxys (filter (fn [nxy] (next? nv nxy data)) [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])]
      (if (seq nxys)
        (apply concat
               (for [nxy nxys]
                 (find-paths ps (conj p nxy) nv nxy data)))
        ps))))


(defn score [th data]
  (let [paths (find-paths [] [] 0 th data)]
    (->> paths
         (map last)
         (into #{})
         count)))

(defn score-2 [th data]
  (count (find-paths [] [] 0 th data)))

(defn part-1 [& t]
  (let [data (get-data t)]
    (->> data
         find-trailheads
         (map (fn [th] (score th data)))
         (apply +))))

(defn part-2 [& t]
  (let [data (get-data t)]
    (->> data
         find-trailheads
         (map (fn [th] (score-2 th data)))
         (apply +))))
