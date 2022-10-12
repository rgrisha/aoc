(ns aoc.day10
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as cset]))

(def parens {\[ \] \{ \} \( \) \< \>})

(def points 
  {\) 3 
   \] 57
   \} 1197
   \> 25137})

 (defn parse-line [cs-in]
  (loop [[c & cr] cs-in state '()]
    (if-let [pair (parens c)]
      (recur cr (cons pair state))
      (if c 
        (if (= c (first state))
          (recur cr (next state))
          {:error  c})
        {:incomplete  state}) )))


(defn run-1 []
  (let [data  (u/get-day-data 10 seq)
        ]
    data
    (->> data
         (map parse-line)
         (filter :error)
         (map :error)
         (map points)
         (reduce +))))

(def scores-2 {\) 1 \] 2 \} 3 \> 4})


(defn run-2 []
  (let [data  (u/get-day-data 10 seq )
        ]
    data
    (->> data
         (map parse-line)
         (filter :incomplete)
         (map :incomplete)
         (map #(reduce (fn [a v] (+ (* a 5) (scores-2 v)) ) 0 %1))
         sort
         (#(nth %1 (quot (count %1) 2)))
         )))
