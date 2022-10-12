(ns aoc.day4
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as cset]))

(defn read-fn [a l]
  (if (= "" (s/trim l))
    (cons [] a)
    (cons (conj (first a) (map (fn [v] (Integer. v)) (s/split (s/trim l) #"[, ]+"))) (rest a ))))


(defn row-won [draw rc]
  (empty? (cset/difference (set rc) draw)))

(defn transpose-table [t]
  (apply map vector t))

(defn table-won? [draw table]
  (or 
    (some (partial row-won draw) table)
    (some (partial row-won draw) (transpose-table table))))
 

(defn first-winning-table [tables rand-seq]
  (loop [rs (take 4 rand-seq) rem-seq (drop 4 rand-seq)]
    (let [next-num (first rem-seq)
          rs (set (cons next-num rs))
          winners (filter (partial table-won? rs) tables)]
      (if (empty? winners)
        (recur (cons next-num rs) (rest rem-seq))
        [next-num rs (first winners)]))))

(defn table-score [win-fn tables rand-seq]
  (let [[wnum wrand wtable] (win-fn tables rand-seq)
        table-nums (->> wtable (apply concat) (into #{}))
        unmarked (cset/difference table-nums wrand)]
    (* wnum (apply + unmarked)) ))


(defn run-1 []
  (let [data (u/get-day-data-reduce 4 read-fn [[]])
        data (reverse data)
        rand-seq (-> data first first)
        tables (rest data)]
    (table-score first-winning-table tables rand-seq)
    ))

(defn last-winning-table [tables rand-seq]
  (loop [remaining-tables (set tables) rs (set (take 4 rand-seq)) rem-seq (drop 4 rand-seq)]
    (let [next-elem (first rem-seq)
          rs (conj rs next-elem) 
          winners (filter (partial table-won? rs) remaining-tables)
          remaining-tables-new (apply disj remaining-tables winners)]
      (if (empty? remaining-tables-new)
        [next-elem rs (first remaining-tables)]
        (recur remaining-tables-new rs (next rem-seq))))))


(defn run-2 []
  (let [data (u/get-day-data-reduce 4 read-fn [[]])
        data (reverse data)
        rand-seq (-> data first first)
        tables (rest data)]
    (table-score last-winning-table tables rand-seq)
    ;(last-winning-table tables rand-seq) 
    ))
