(ns aoc.day12
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as cset]))

(defn gen-parts [d]
  (->> (into d (map (fn [[a b]] [b a]) d))
       (filter (fn [[a _]] (not= a "end"))) 
       (filter (fn [[_ b]] (not= b "start"))) 
       (reduce (fn [a [k v]] (update a k #(conj (or %1 []) v))) {})) )

(defn filter-next-paths-1 [path nps]
  (let [small-paths (filter #(re-matches #"[a-z][a-z]" %1) path)
        ps (into #{} small-paths)]
    (filter #(not (contains? ps %1)) nps)))

(defn filter-next-paths-2 [path nps]
  (when (> (count path) 15 ) (throw (Exception. "path")))
  (let [small-paths (filter #(re-matches #"[a-z][a-z]" %1) path)
        bad? (fn [p]
               (let [bigger-freqs (remove #{1} (vals (frequencies (cons p small-paths))))
                     bigger-freqs (sort > bigger-freqs)]
                 ;(println "bigger-freqs" bigger-freqs small-paths path nps p)
                 (or
                   (= '(2 2) bigger-freqs)
                   (= '(3)   bigger-freqs)) ))]
    (filter (comp not bad?) nps)))

(defn make-gen-paths [paths path-filter-fn]  
  (fn gen-paths [acc-paths acc-path cn]
    ;(println acc-path)

    (if (empty? (path-filter-fn acc-path [cn]))
      nil

      (let [deep-paths (path-filter-fn acc-path (paths cn))
            red-fn (fn [path acc v] 
                     (if (= v "end")
                       (conj acc (conj path v))
                       (into acc (filter (comp not nil?) (gen-paths [] path v))) )) ]
        (if (empty? deep-paths)
          nil 
          (reduce (partial red-fn (conj acc-path cn)) acc-paths deep-paths))))))

(defn run-1 []
  (let [data  (u/get-day-data 12 #(s/split %1 #"-"))
        paths (gen-parts data)
        gen-paths-fn (make-gen-paths paths filter-next-paths-1) ]
    (count (gen-paths-fn [] [] "start" ))))

(defn run-2 []
  (let [data  (u/get-day-data 12 #(s/split %1 #"-"))
        paths (gen-parts data)
        gen-paths-fn (make-gen-paths paths filter-next-paths-2) ]
    (count (gen-paths-fn [] [] "start" ))))
