(ns aoc.day2
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as cset]))


(defn next-pos-1 [{:keys [h d]} [cmd v]]
  (case cmd
    :forward {:d d :h (+ h v)}
    :down    {:d (+ d v) :h h}
    :up      {:d (- d v) :h h}
    {:h h :d d}))

(defn follow-pos [next-pos-fn initial data]
  (reduce next-pos-fn initial data))

(defn run-1 []
  (let [data (u/get-day-data 2 (fn [v] (let [[k v] (s/split v #" ")] [(keyword k) (Integer. v)])  ))
        last-pos (follow-pos next-pos-1 {:h 0 :d 0} data)]
    (* (:h last-pos) (:d last-pos)) ))

(defn next-pos-2 [{:keys [h d a]} [cmd v]]
  (case cmd
    :forward {:d (+ d (* a v)) :h (+ h v) :a a}
    :down    {:d d :h h :a (+ a v)} ;{:d (+ d v) :h h :a (+ a v)}
    :up      {:d d :h h :a (- a v)} ;{:d (- d v) :h h :a (- a v)}
    {:h h :d d :a a}))

(defn run-2 []
  (let [data (u/get-day-data 2 (fn [v] (let [[k v] (s/split v #" ")] [(keyword k) (Integer. v)])))
        last-pos (follow-pos next-pos-2 {:h 0 :d 0 :a 0} data)]
    (* (:h last-pos) (:d last-pos)) ))

