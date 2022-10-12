(ns aoc.day14
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as cset]))


(defn get-data []
  (let [data  (u/get-day-data 14 #(s/split %1 #" -> ") )
        chain (ffirst data)
        data (drop 2 data)
        ]
   [(seq chain) (into {} (map (fn [[a b]] [(seq a) (first b)]) data))]))

(defn scan [instr chain]
  (let [r   (map (fn [i c1 c2] [0 i (instr (list c1 c2))]) (drop 1 (range)) chain (next chain))
        mrg (sort (fn [[p1 n1 _] [p2 n2 _]] (if (= n1 n2) (< p1 p2) (< n1 n2)) ) 
                  (concat 
                         r
                         (map (fn [a b] [1 a b]) (range) chain)))
        ]
    (map #(nth %1 2) mrg)))

(defn scan-n [instr -chain -n]
  (loop [chain -chain n -n]
    (if (zero? n)
      chain
      (recur (scan instr chain) (dec n)))

    )
  )

(defn run-1 []
  (let [[chain instr] (get-data)
        chain-done (scan-n instr chain 40)
        frqs  (vals  (frequencies chain-done))
        frqss (sort frqs) 
        
        ]
    frqss
    (- (last frqss) (first frqss))
    )
  )
