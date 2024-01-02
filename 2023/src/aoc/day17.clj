(ns aoc.day17
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))


(defn line-fn [s]
  (mapv (fn [e] (- (int e) (int \0))) s))

(defn get-data [& args] 
  (u/get-day-data 17 line-fn (first args))) 

(get-data :test)

(defn with-value [rmap v [y x] d]
  (try
    [[y x] d (+ v ((rmap y) x))]
    (catch IndexOutOfBoundsException _ nil)))

(defn get-neighbours [rmap maxx maxy visited [y x] prev-value invalid-dir]
  ;(println "contains? visited" [y x] visited)
  (remove 
    (fn [[[y x] d _]] (or (nil? y) (contains? visited [y x]) (= d invalid-dir)))
    [(with-value rmap prev-value [(dec y) x] :u)
     (with-value rmap prev-value [(inc y) x] :d)
     (with-value rmap prev-value [y (inc x)] :r) 
     (with-value rmap prev-value [y (dec x)] :l)])) 


(defn find-path [maxy maxx rmap]
  (loop [current [0 0] visited #{} dirs '() map-state {[0 0] 0}]
    (let [[a b c] (take 3 dirs)
          invalid-dir (if (= a b c) a nil)
          curr-value (map-state current)
          nghs (get-neighbours rmap maxx maxy visited current curr-value invalid-dir)]
      (println "current " current " neighbours: " nghs "dirs" dirs)
      (if (seq nghs)
        (let [new-map-state (reduce (fn [a [[y x] _ v]] (update a [y x] (fn [pv] (if (nil? pv) v (min v pv))))) map-state nghs)
              min-neigh (reduce (fn [[p1 _ _ :as n1] [p2 _ _ :as n2]] (if (< (new-map-state p1) (new-map-state p2)) n1 n2)) nghs)
              [[ny nx] nd _] min-neigh]
          (recur [ny nx] (conj visited current) (cons nd dirs) new-map-state))
        map-state)))) 

    
(defn part-1 [& args] 
  (let [rmap (get-data (first args))
        maxy (count rmap)
        maxx (count (first rmap))]
    (find-path maxy maxx rmap)))  

