(ns aoc.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn get-data-from-file [file f line-fn]
  (let [s  (slurp (str "resources/" file))
        ls (s/split-lines s)]
    (line-fn f ls)))

(defn get-day-data 
  ([number f t]
   (get-data-from-file (str "day-" number "-" (name t) ".txt") f map))
  ([number f]
   (get-data-from-file (str "day-" number ".txt") f map)))
      
(defn get-day-data-reduce
  ([number f init-elem  t]
   (get-data-from-file (str "day-" number "-" (name t) ".txt") f (fn [f coll] (reduce f init-elem coll))))
  ([number f init-elem]
   (get-data-from-file (str "day-" number ".txt") f (fn [f coll] (reduce f init-elem coll)))))
