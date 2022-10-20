(ns aoc.day14
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as cset]))


(defn get-data []
  (let [data  (u/get-day-data 14 #(s/split %1 #" -> "))
        chain (ffirst data)
        data (drop 2 data)]
        
   [(seq chain) (into {} (map (fn [[a b]] [(seq a) (first b)]) data))]))


(defn chain-replace [chain instr]
  (map (fn [cc nc] (if-let [repl (get instr [cc nc])] [cc repl] [cc])) chain (next chain)))
  
(defn chain-replace [instr chain]
  (first
    (reduce (fn [[a n] v] (if-let [r (get instr [v (first n)])]
                            [(conj (conj a v) r) (next n)] 
                            [(conj a v)          (next n)]))
            [[] (next chain)] 
            chain))) 
     
    
(defn run-1 []
  (let [[chain instr] (get-data)
        instr (into {} (map (fn [[[c1 c2] v]] [[c1 c2] v]) instr)) 
        ;_ (println "c i " chain "---" instr "----" (chain-replace instr chain))      
        chain (nth (iterate (partial chain-replace instr) chain) 10)
        get-diff (fn [l] (- (second (last l)) (second (first l))))]
    (->> chain      
         frequencies 
         (sort-by second) 
         get-diff)))
    
    
