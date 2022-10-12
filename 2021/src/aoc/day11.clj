(ns aoc.day11
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as cset]))

(defn get-neighbours [field [y x]]
  (->> (for [dy [y (dec y) (inc y)] dx [x (dec x) (inc x)]]
         [(get-in field [dy dx]) dy dx])
       next
       (filter first)
       (map (fn [[_ y x]] [y x]))))
       

(defn inc-field [field]
  (mapv #(mapv inc %1) field)) 

(defn debug-pr [a b] (println a b) b)

(defn print-field [txt fld]
  (println txt)
  (doseq [f fld]
    (println (s/join "" f)) )
  fld)

(defn run-step [in-field]
  (loop [field (inc-field in-field) flash-set #{} n 100]
    (let [;_ (print-field "in" field)
          flashes (for [y (range (count field)) x (range (count (get field y)))
                        :when (> (get-in field [y x]) 9)] [y x])
          fl-set (into #{} flashes)
          field  (reduce (fn [a [y x]] (update-in a [y x] (constantly 0))) field fl-set)
          neib-1 (apply concat (map (partial get-neighbours field) flashes))
          ;neib-set (cset/difference (into #{} neib-1) fl-set)
          new-field (reduce (fn [a [y x]] (update-in a [y x] inc)) field neib-1)]
      ;(print-field "new field" new-field)
      (if (or (zero? n) (empty? fl-set))
        [(count flash-set) flash-set (reduce (fn [a v] (update-in a v (constantly 0))) new-field flash-set)]
        (recur new-field (cset/union flash-set fl-set) (dec n))))))

(defn count-flashes [field n]
  (loop [[ c _ field] [0 nil field] cnt n acc 0]
    (if (zero? cnt)
      (do
        (println acc)
        (print-field "aff" field) 
        )
      (recur (run-step field) (dec cnt) (+ acc c)))))

(defn all-flash? [field]
  (every? #(every? zero? %1) field))

(defn count-until-all-flash [field]
  (loop [[_ _ field] [0 nil field] cnt 0]
    (if (all-flash? field)
      (do
        (println "all flash at " cnt))
      (recur (run-step field) (inc cnt) ))))

(defn load-data []
  (let [data  (u/get-day-data 11 #(mapv (fn [v] (Integer. (str v))) (seq %1)))
        data (vec data) 
        ]
    data))

(defn run-2 []
  (let [data  (u/get-day-data 11 #(mapv (fn [v] (Integer. (str v))) (seq %1)))
        data (vec data) 
        ]
    data
    (count-until-all-flash data )
    ))


