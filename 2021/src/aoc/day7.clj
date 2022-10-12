(ns aoc.day7
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as cset]))

(defn parse-line [l]
  (->> l
       ((fn [s] (s/split s #",")))
       (map (fn [e] (Long. e)))))

(defn diff-1 [a b]
   (if (< a b ) (- b a) (- a b)))

(defn diff-2 [a b]
  (let [adj (fn [v] (quot (* v (inc v)) 2))
        a- (fn [a b] (adj (- a b)))]
    (if (< a b ) (a- b a) (a- a b))))


(defn get-fuel-for-pos [posses diff-fn p]
  (reduce #(+ %1 (diff-fn %2 p)) 0 posses))


(defn run-1 []
  (let [data  (u/get-day-data 7 parse-line)
        data (first data)]
    (apply min (map (partial get-fuel-for-pos data diff-1) data))
    ))

(defn run-2 []
  (let [data  (u/get-day-data 7 parse-line)
        data (first data)
        all-pos (range 0 (apply max data)) ]
    (apply min (map (partial get-fuel-for-pos data diff-2) all-pos))
    ))

; bad: 100149945
