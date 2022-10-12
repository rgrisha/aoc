(ns aoc.day17
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as cset]))

;target area: x=29..73, y=-248..-194
(def target-area {:x {:from 29 :to 73} :y {:from -248 :to -194}})

(def min-x 29)
(def min-y -248)
(def max-x 73)

(defn is-in-target? [x y]
  (and (>= x 29)
       (<= x 73)
       (>= y -248)
       (<= y -194)))

(defn is-in-target-x [x]
  (and (>= x 29))
       (<= x 73))

(defn status? [x y]
  (cond (or (< y min-y) (> x 73)) :out
        (is-in-target? x y) :ok
        :else :in))

(defn move-x-until-target [x']
  (loop [x x' acc 0]
    (if (zero? x)
      (if (< acc min-x) x' nil)
      (recur (dec x) (+ acc x)))) )

(defn find-min-x []
  (->> (range 1 (inc min-x))
       (map move-x-until-target)
       (filter (comp not nil?))
       (apply max)))


(defn gen-positions []
  (for [y' (range min-y 248) x' (range (inc (find-min-x)) (inc max-x))]
    (do
      (println "y" y')
      (loop [x-acc 0 y-acc 0 xd x' yd y']
        (case (status? x-acc y-acc)
          :out nil
          :ok [x' y']
          (recur (+ x-acc xd) (+ y-acc yd) (if (zero? xd) xd (dec xd)) (dec yd)))))))

;247 too low

(defn gen-y-pos [y']
  (loop [acc 0 accl [] dy y']
    (if (< acc min-y)
      accl
      (recur (+ acc dy) (conj accl (+ acc dy)) (dec dy))) ))

(defn run-2 []
  (->> (gen-positions)
       (filter (comp not nil?)) 
       frequencies
       keys
       count
       )
     
  )
