(ns aoc.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn get-day-data 
  ([number f t]
   (let [filename (str "day-" number (if t (str "-" (name t)) "")  ".txt")]
     (with-open [rdr (-> filename io/resource io/reader)]  
       (mapv f (line-seq rdr)))))
  ([number f]
   (get-day-data number f nil)))

(defn get-day-data-reduce
  ([number f init-elem t]
   (let [filename (if t (str "day-" number "-" (name t) ".txt")
                        (str "day-" number              ".txt"))]

     (with-open [rdr (-> filename io/resource io/reader)]
       (reduce f init-elem (line-seq rdr)))))
    
  ([number f init-elem]
   (get-day-data-reduce number f init-elem nil)))
