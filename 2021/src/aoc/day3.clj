(ns aoc.day3
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as cset]))

(defn count-commons-line [a l]
  (map (fn [aa ll] (if (= ll 1) (inc aa) aa) ) a l))

(defn count-commons [d]
  (let [init (repeat (count (first d)) 0)]
    (reduce count-commons-line init d)))

(defn eval-common [f l]
  (let [d (map f l)]
    (Integer/parseInt (s/join d) 2)))

(defn eval-commons [h-cnt l]
  (let [commons-1 (eval-common (fn [v] (if (> v h-cnt) 1 0)) l)
        commons-0 (eval-common (fn [v] (if (> v h-cnt) 0 1)) l)]
    (* commons-1 commons-0)))

(defn map-in-line [l]
  (map 
    (fn [ll] (if (= ll \1) 1 0))
    (seq l)))

(defn run-1 []
  (let [data (u/get-day-data 3 map-in-line)
        in-cnt (count data)
        h-cnt (quot in-cnt 2)]
    (->> (count-commons data)
         (eval-commons h-cnt))))


(defn count-common-bits [n ls]
  (reduce (fn [[a0 a1] l] (if (= 0 (nth l n)) [(inc a0) a1] [a0 (inc a1)])) [0 0] ls))


(defn get-common-series [cmp-fn take-if-eq n d]
  (let [cb (count-common-bits n d)
        common-bit (cond (apply = cb) take-if-eq 
                         (apply cmp-fn cb) 0 
                         :default 1)]
    (println "cb " cb "common bit" common-bit)
    (filter (fn [l] (= (nth l n ) common-bit)) d)) )

(defn find-rating [cmp-fn take-if-eq data]
  (loop [n -1 d data]
    (if (= 1 (count d))
      (Integer/parseInt (s/join (first d)) 2)
      (recur (inc n) (get-common-series cmp-fn take-if-eq (inc n) d)))
    
    )) 

(defn run-2 []
  (let [data (u/get-day-data 3 map-in-line)
        ]
    (*
     (find-rating > 1 data) 
     (find-rating < 0 data))))

