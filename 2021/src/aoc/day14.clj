(ns aoc.day14
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as cset]))


(defn get-data []
  (let [data  (u/get-day-data 14 #(s/split %1 #" -> "))
        chain (ffirst data)
        data (drop 2 data)]
        
   [(seq chain) (into {} (map (fn [[a b]] [(seq a) (first b)]) data))]))

  
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
        get-diff (fn [l] (- (second (last l)) (second (first l))))
        _ (println "length of chain: " (count chain))]
    (->> chain      
         frequencies 
         (sort-by second) 
         get-diff)))

(defn next-pair-gen [instr a [p1 p2] cnt]
  (if-let [v (get instr [p1 p2])]
    (-> a
      (update [p1 v] (fn [ov] (+ (or ov 0) cnt)))
      (update [v p2] (fn [ov] (+ (or ov 0) cnt)))) 
    a))
  

(defn counted-iteration [instr pair-counts]
  (reduce (fn [a [k v]] (next-pair-gen instr a k v)) {} pair-counts))

(defn +? [v] (fn [ov] (+ (or ov 0) v)))

(defn run-2 []
  (let [[chain instr] (get-data)
        instr (into {} (map (fn [[[c1 c2] v]] [[c1 c2] v]) instr)) 
        ;_ (println "c i " chain "---" instr "----" (chain-replace instr chain))      
        repl-fn (partial chain-replace instr)
        chain1 (repl-fn chain) 
        chain2 (repl-fn chain1)  
        pair-counts (frequencies (map vector chain (rest chain)))
        it1 (counted-iteration instr pair-counts)
        it2 (counted-iteration instr it1)

        final-iteration (nth (iterate (partial counted-iteration instr) pair-counts) 40)
        stats (reduce (fn [a [[c1 c2] n]] (-> a (update-in [c1 :1] (+? n)) (update-in [c2 :2] (+? n)))) {} final-iteration)
        stats (map (fn [[c m]] [c (max (:1 m) (:2 m))] ) stats)
        sorted (sort-by second stats)]
    (- (second (last sorted)) (second (first sorted)))))

