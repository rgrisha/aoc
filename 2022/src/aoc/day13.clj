(ns aoc.day13
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as s]))


(defn get-data [& ds]
  (let [filename (if-let [sfx (first ds)]
                   (str "day-" 13"-" (name sfx) ".txt")
                   (str "day-" 13 ".txt"))
        data (-> filename io/resource slurp)
        data (s/split data #"\n\n")
        data (mapv s/split-lines data)]
    (mapv (fn [d] (mapv edn/read-string d)) data))) 


(defn compare-values [l r]
  (println "parent " l r)
  (cond (and (sequential? l) (sequential? r)) 
        (loop [l l r r]
          (println "loop " l r)
          (if (and (nil? l) (nil? r))
            ;(recur (next l) (next r))
            nil
            (let [result (compare-values (first l) (first r))]
              (if (nil? result)
                (recur (next l) (next r))
                result))))

        (and (sequential? l) (number? r)) 
        (compare-values l [r])

        (and (number? l) (sequential? r)) 
        (compare-values [l] r)

        (and (number? l) (number? r))
        (if (= l r)
          nil
          (< l r))

        (and (nil? l) (not (nil? r)))
        true

        (and (not (nil? l)) (nil? r))
        false

        (and (nil? l) (nil? r))
        nil  

        :else
        (throw (Exception. (str "Should not come here " (type l) " " (type r) " l: " l " r: " r)))))

(defn run-1 [& ds]
  (let [data (get-data (first ds))
        m (map-indexed (fn [i [l r]] [(inc i) (compare-values l r)]) data)
        m (filter second m)]
    (reduce + (map first m)))) 
