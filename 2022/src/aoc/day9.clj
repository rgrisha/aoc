(ns aoc.day9
  (:require [aoc.utils :as u]
            [clojure.string :as s]))
             



(defn get-data [& ds]
  (u/get-day-data 9 
                  (fn [line] (let [[d n] (s/split line #" ")]
                               [(-> d s/lower-case keyword) (Integer/parseInt n)])) 
                  (first ds)))

(defn calculate-move-pos [[x y] d]
  (case d
        :u [x (dec y)]
        :d [x (inc y)]
        :l [(dec x) y]
        :r [(inc x) y]))

(defn normalize-diagonal-move [[x y] d [hx hy]]
  (case d
        :u [hx y]
        :d [hx y]
        :l [x hy]
        :r [x hy]))

(defn abs- [a b] (abs (- a b)))

(defn move [[[tx ty] hp all-tail-pos] [d n]]
  (loop [[tx ty :as tp] [tx ty] hp hp atp all-tail-pos n n]
    ;(println "positions: t h" tp hp)
    (if (= 0 n)
      [tp hp atp]
      (let [[nhx nhy :as nhp] (calculate-move-pos hp d)]
        (cond (and (< (abs- nhx tx) 2) (< (abs- nhy ty) 2))                    
              (recur tp nhp atp (dec n))


              :else 
              (let [ntp (calculate-move-pos tp d)
                    ntp (normalize-diagonal-move ntp d nhp)]
                (recur ntp nhp (conj atp ntp) (dec n)))))))) 

(defn run-1 [& ds]
  (let [data (get-data (first ds))
        [_ _ all-tail-pos] (reduce move [[0 0] [0 0] [0 0]] data)]
    (count (into #{} all-tail-pos))))
                 

(comment
  ; 5884 too high
  (run-1 :test))


