(ns aoc.day6
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as cset]))


(defn read-fn [acc line]
  (let [fi (first acc)]
    (if (empty? line)
      (cons [] acc)
      (cons (conj fi line) (rest acc)))))

(defn question-num [qs]
  (->> qs
       (map seq) 
       (apply concat)
       (into #{})
       count))

(defn run-1 []
  (let [in-data (u/get-day-data-reduce 6 read-fn [])]
    (apply +  (map question-num in-data))))

(defn num-2 [qs]
  (->> qs
       (map seq)
       (map (fn [e] (into #{} e)))
       (apply cset/intersection)
       count)) 


(defn run-2 []
  (let [in-data (u/get-day-data-reduce 6 read-fn [])]
    (apply + (map num-2 in-data))))

 

