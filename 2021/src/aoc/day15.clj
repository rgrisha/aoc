(ns aoc.day15
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as cset]))

(defn parse-line [line]
  (->> line
       seq
       (mapv #(Integer. (str %1)))))

;getting data for Dijkstra algo test
(defn get-data-for-dj-test []
  (let [data (u/get-day-data 15 parse-line )
        data (vec data)]
    (into {}
      (for [y (range (count data)) x (range (count (get data y)))]
        (let [p1 (get-in data [(inc y) x])
              p2 (get-in data [y (inc x)])]
          (do
          [[y x]
           (cond-> {}
             p1 (assoc [(inc y) x] p1)
             p2 (assoc [y (inc x)] p2))]))))))

(defn get-data-z []
  (let [data (u/get-day-data 15 parse-line )
          data (vec data)]
    data)  )

(defn get-data []
  (let [data (u/get-day-data 15 parse-line )
        data (vec data)]
    (->
      (into {}
        (for [y (range (count data)) x (range (count (get data y)))]
          [[y x] {:v (get-in data [y x])}]))
      (update [0 0] #(assoc %1 :path []))) ))

(defn update-point [p {:keys [v path]}]
  (let [parent-path path
        nv v]
    (fn [{:keys [v path]}] 
      {:v (+ v nv) :path (conj parent-path p)})))

(defn min-point-2-directions [data [y x]]
  (let [p1 [(dec y) x ]
        p2 [y (dec x)]
        p1n (get data p1)
        p2n (get data p2)]
    ;(println [y x] p1 p2 p1n p2n)
    (cond
      (nil? p1n)             (update data [y x] (update-point [y x] p2n))
      (nil? p2n)             (update data [y x] (update-point [y x] p1n))
      (< (:v p2n) (:v p1n))  (update data [y x] (update-point [y x] p2n))
      :else                  (update data [y x] (update-point [y x] p1n)))))

(defn min-point [data [y x]]
  (let [p1 [(dec y) x ]
        p2 [y (dec x)]
        p1n (get data p1)
        p2n (get data p2)]
    ;(println [y x] p1 p2 p1n p2n)
    (cond
      (nil? p1n)             (update data [y x] (update-point [y x] p2n))
      (nil? p2n)             (update data [y x] (update-point [y x] p1n))
      (< (:v p2n) (:v p1n))  (update data [y x] (update-point [y x] p2n))
      :else                  (update data [y x] (update-point [y x] p1n)))))

(defn get-new-layer [data layer]
  (let [new-points (mapcat (fn [[y x]] [[(inc y) x] [y (inc x)]]) layer)
        new-points (filter (fn [[y x]] (get data [y x])) new-points)
        new-points (into #{} new-points)  
        data (reduce min-point data new-points)]
    [data new-points]))

(defn calc-min-path [data'']
  (loop [data data'' layer [[0 0]]]
    (let [[data' new-layer] (get-new-layer data layer)]
      (if (empty? new-layer)
        (do
          (-
            (:v (get data (first layer)))
            (:v (get data [0 0]))))
        (recur data' new-layer)))))

;620 too high
(defn run-1 []
  (let [data (get-data) ]
    (calc-min-path data)))
