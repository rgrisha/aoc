(ns aoc.day8
  (:require [aoc.utils :as u]))

(comment
  (get-data :test))

(defn get-data [& s]
  (u/get-day-data 8 (fn [line] (mapv #(- (int %) (int \0)) line)) (first s)))


(defn visible? [data max-x max-y x y]
  (let [get-tree (fn [x y] (nth (nth data y) x))
        tree (get-tree x y)
        row (nth data y)]
    (or (= x 0) 
        (= x (dec max-x)) 
        (= y 0) 
        (= y (dec max-y))
        (every? #(< % tree) (for [xa (range x)] (nth row xa)))
        (every? #(< % tree) (for [xa (range (inc x) max-x)] (nth row xa)))
        (every? #(< % tree) (for [ya (range y)] (get-tree x ya)))
        (every? #(< % tree) (for [ya (range (inc y) max-y)] (get-tree x ya))))))
   


(defn run-1 [& ds]
  (let [data (get-data (first ds))
        max-x (count (first data))
        max-y (count data)
        visible? (partial visible? data max-x max-y)]
    (reduce +
      (for [y (range max-y)]
        (reduce (fn [a x] (if (visible? x y) (inc a) a)) 0 (range max-x))))))
    
(defn scenic-score [data max-x max-y]
  (fn [x y]
    (let [get-tree (fn [x y] (nth (nth data y) x))
          tree (get-tree x y)
          row (nth data y)
          red-fn (fn [a v] (if (< v tree) (inc a) (reduced (inc a))))]
      (* 
         (reduce red-fn 0 (for [ya (range (dec y) -1 -1)] (get-tree x ya)))
         (reduce red-fn 0 (for [xa (range (dec x) -1 -1)] (nth row xa)))
         (reduce red-fn 0 (for [ya (range (inc y) max-y)] (get-tree x ya)))
         (reduce red-fn 0 (for [xa (range (inc x) max-x)] (nth row xa)))))))


(comment
  (run-2))

(defn run-2 [& ds]
  (let [data (get-data (first ds))
        max-x (count (first data))
        max-y (count data)
        scenic-score-fn (scenic-score data max-x max-y)]
    (reduce max      
      (for [x (range 1 (dec max-x))
            y (range 1 (dec max-y))]
        (scenic-score-fn x y)))))

