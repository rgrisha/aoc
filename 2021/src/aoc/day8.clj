(ns aoc.day8
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as cset]))

(defn parse-segment [s]
  (map (comp keyword str) (seq s)))

(defn parse-part [p]
  (map parse-segment (s/split p #" ")))

(defn parse-line [l]
  (let [ps (s/split l #" \| ")]
    (map parse-part ps)))


(defn run-1 []
  (let [data  (u/get-day-data 8 parse-line)]
    (->> data
         (map second)
         (map (fn [v] (filter (fn [vv] (#{2 3 4 7} (count vv))) v)))
         (map count)
         (reduce +))))

(def rules
  [(fn [[a b c & _ :as z]] (when (= (count z) 3)
                              {a [0 2 5]
                               b [0 2 5]
                               c [0 2 5]}))
   (fn [[a b c d & _ :as z]] (when (= (count z) 4)
                               {a [1 2 3 5]
                                b [1 2 3 5]
                                c [1 2 3 5]
                                d [1 2 3 5]}))
   (fn [[a b & _ :as z]] (when (= (count z) 2)
                             {a [2 5]
                              b [2 5]}))
   (fn [[a b c d e f g & _ :as z]] (when (= (count z) 10)
                              {a [0 1 2 3 4 5 6]
                               b [0 1 2 3 4 5 6]
                               c [0 1 2 3 4 5 6]
                               d [0 1 2 3 4 5 6]
                               e [0 1 2 3 4 5 6]
                               f [0 1 2 3 4 5 6]
                               g [0 1 2 3 4 5 6] }))
   ])

(def digits 
  {#{0 1 2 4 5 6}   0
   #{2 5}           1
   #{0 2 3 4 6}     2
   #{0 2 3 5 6}     3
   #{1 2 3 5}       4
   #{0 1 3 5 6}     5
   #{0 1 3 4 5 6}   6
   #{0 2 5}         7
   #{0 1 2 3 4 5 6} 8
   #{0 1 2 3 5 6}   9} )




(defn run-2 []
  (let [data  (u/get-day-data 8 parse-line)]
    data
    ))

