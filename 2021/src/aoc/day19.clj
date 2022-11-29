(ns aoc.day19
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as cset]
            [clojure.edn :as edn]))

(defn line-fn [[a t] line]
  (if (= line "")
    [a t]
    (if (re-matches #"^---.*" line)
      (if (empty? t) 
        [a t]
        [(conj a t) []])
      (let [nums (s/split line #",")] 
        [a (conj t (mapv (fn [a] (Integer/parseInt a)) nums))]))))
   
(defn get-data []
  (let [[a t] (u/get-day-data-reduce 19 line-fn [[] []] :test)]
    (conj a t))) 

(def operations
  (for [permutation [[0 1 2][0 2 1][1 2 0][1 0 2][2 0 1][2 1 0]] 
        sign        [[0 0 0][0 0 1][0 1 0][0 1 1][1 0 0][1 0 1][1 1 0][1 1 1]]] 
    [permutation (mapv {0 1 1 -1} sign)]))

(defn has-common-beacons? [[ops beacons]]
  (let [freqs (frequencies beacons)]
    (some (fn [[frq cnt]] (when (>= cnt 12) [ops frq])) freqs)))

(defn all-beacons [sc1 sc2]
  (for [[pm sgn] operations] 
    [[pm sgn] (for [a sc1 
                    b sc2] 
                (for [i [0 1 2]]
                  (+ (get a i) (* (get sgn i) (get b (get pm i))))))]))

(defn scanner-nearby [sc1 sc2]
  (some has-common-beacons? (all-beacons sc1 sc2)))

        
(defn find-all-scanners []
  (let [data (get-data)]
    (loop [layers [] layer [0] remaining-to-find (into #{} (range 1 (count data)))]
      (println "layer" layer "rem " remaining-to-find) 
      (if (empty? remaining-to-find)    
        layers
        (let [found-scanners (for [li layer 
                                   msi remaining-to-find
                                   :let [_ (println "searching for overlap for " li msi)
                                         other-scanner (scanner-nearby (get data li) (get data msi))]
                                   :when other-scanner]
                               [li msi other-scanner])
              new-layer (into #{} (map #(nth % 1) found-scanners)) 
              new-remaining (cset/difference remaining-to-find new-layer)] 
          (if (empty? new-layer)
            layers
            (recur (conj layers new-layer) new-layer new-remaining)))))))
                            
          
           
                  
      
    
      
        
        
