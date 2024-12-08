(ns aoc.day8
  (:require [clojure.string :as s]
            [aoc.utils      :as u]
            [clojure.set    :as cs]))

(defn line-fn [s]
  (mapv identity s))

(defn to-map [data]
  (reduce-kv (fn [m y line]
               (reduce-kv (fn [mm x v]
                            (if (= v \.)
                              mm
                              (update mm v (fn [ov] (cons [x y] ov)))))
                          m
                          line))
             {}
             data))

(defn get-data [& t]
  (let [data (u/get-day-data 8 line-fn (first t))]
    {:data (to-map data) :h (count data) :w (count (first data))}))

;(antinodes [[4 4] [7 3] [5 2] [8 1]] 10 10)
(defn antinodes [frs w h]
  (for [[ax ay :as a] frs
        [bx by :as b] frs
        :when (not= a b)
        :let [nx (+ ax (- ax bx))
              ny (+ ay (- ay by))]
        :when (and (>= nx 0) (< nx w))
        :when (and (>= ny 0) (< ny h))]
    [nx ny]))

(defn all-antinodes [a-fn w h data]
  (loop [acc [] frss (map second data)]
    (if (nil? frss)
      acc
      (let [an (a-fn (first frss) w h)]
        (recur (into acc an) (next frss))))))

(defn part-1 [& t]
  (let [{:keys [data w h]} (get-data (first t))]
    (->> data
         (all-antinodes antinodes w h)
         (into #{})
         count)))

;(antinodes-2  [[5 2] [4 4]] 12 12)
(defn antinodes-2 [frs w h]
  (for [[ax ay :as a] frs
        [bx by :as b] frs
        d (range 0 (max w h))
        :when (not= a b)
        :let [dx (- ax bx)
              dy (- ay by)]

        :let [nx (+ ax (* d (- ax bx)))
              ny (+ ay (* d (- ay by)))]

        :when (and (>= nx 0) (< nx w))
        :when (and (>= ny 0) (< ny h))]
    [nx ny]))

(defn part-2 [& t]
  (let [{:keys [data w h]} (get-data (first t))]
    (->> data
         (all-antinodes antinodes-2 w h)
         (into #{})
         count)))


