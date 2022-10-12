(ns aoc.day6
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as cset]))

(defn parse-line [l]
  (->> l
       ((fn [s] (s/split s #",")))
       (map (fn [e] (Long. e)))))

(defn fish-spawn-1 [fish]
  (if (= fish 0)
    [6 [8]] 
    [(dec fish) nil]))

(defn next-gen [fish-fn gen]
  (let [next-gen-fn (fn [[a anew] v] (let [[f new-f] (fish-fn v)] [(conj a f) (conj anew new-f)]))
        next-gen (reduce next-gen-fn [[] []] gen)]
    (vec
      (concat 
        (first next-gen) 
        (filter (comp not nil?) (flatten (second next-gen)))))))

(defn fish-gen-optimised [gen]
  (let [fish-0-cnt (or (get gen 0) 0)
        to-map (fn [l] (into {} l)) 
        genl (into [] (dissoc gen 0))
        new-gen (map (fn [[g n]] [(dec g) n]) genl)]
    (println "newg " new-gen)
    (-> new-gen
        to-map
        (update 6 (fn [v] (+ (or v 0) fish-0-cnt)))  
        (assoc 8 fish-0-cnt))))

(defn run-1 []
  (let [data  (u/get-day-data 6 parse-line)
        data (first data)]
    (count  (first  (drop 80  (iterate  (partial next-gen fish-spawn-1) data )))) ))

(defn run-2 []
  (let [data  (u/get-day-data 6 parse-line)
        data (first data)]
    
    (apply 
      + 
      (vals
        (first (drop 256 (iterate fish-gen-optimised (frequencies data ))))))))
