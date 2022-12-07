(ns aoc.day7
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.walk :as walk]))

(defn parse [line]
  (let [sp (s/split line #" ")
        fsp (first sp)
        ssp (second sp)
        tsp (first (drop 2 sp))]
    (cond (= "$" fsp)
          (when (= ssp "cd")
            (if (= tsp "..")
              '(:up)
              (cons :dir [tsp])))
            
          
          (= "dir" fsp)
          nil

          :else
          (cons :file [(Integer/parseInt (first sp)) (second sp)]))))

(comment
  (get-data)
  (run-1 :test))

(defn get-data [& ds]
  (filter (complement nil?)
    (u/get-day-data 7 parse (first ds))))

(defn build-tree [log state]
  (loop [log log state state]
    (let [[a b c] (first log)
          [dir-name dirs files] state] 
      (cond (or (nil? log ) (= a :up))
            [(next log) state]

            (= a :file)
            (recur (next log) [dir-name dirs (conj files [b c])])

            (= a :dir)
            (let [[new-log new-state] (build-tree (next log) [b [] []])]
              (recur new-log [dir-name (conj dirs new-state) files])) 
          
            :else
            (throw (Exception. (str "Should not reach this line " a " log " log)))))))

(defn get-size-of-directory [[_ dirs files]]
  (+ (reduce (fn [a v] (+ a (first v))) 0 files)
     (reduce (fn [a v] (+ a (get-size-of-directory v))) 0 dirs)))

(defn walk-sizes [ff dirs acc]
  (let [dir-sizes (map get-size-of-directory dirs)
        new-acc (concat acc (filter ff dir-sizes))]
     (reduce (fn [a v] (walk-sizes ff v a)) new-acc (map second dirs)))) 
   
  
(defn run-1 [ds]
  (let [log (get-data ds)
        tree (second (second (build-tree log [nil [] []])))
        sizes (walk-sizes #(<= % 100000) tree [])]
   (apply + sizes)))
  
(defn run-2 [ds] 
  (let [log (get-data ds)
        tree (second (second (build-tree log [nil [] []])))
        unused-space (- 70000000 (get-size-of-directory [nil tree []]))
        _ (println "unused space: " unused-space)
        sizes (sort (walk-sizes (constantly true) tree []))
        suitable-dirs (drop-while #(< (+ unused-space %) 30000000) sizes)]
    (first suitable-dirs))) 
