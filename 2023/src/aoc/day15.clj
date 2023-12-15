(ns aoc.day15
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn line-fn [s]
  (s/split s #","))


(defn get-data [& args] 
  (apply concat (u/get-day-data 15 line-fn (first args)))) 

(defn get-hash [init s]
  (reduce
    (fn [a c] (-> a 
                  (+ (int c))
                  (* 17)
                  (rem 256)))
              
    init
    s))

(get-hash 0 "ot")

(defn part-1 [& args]
  (reduce
    + 
    0
    (map #(get-hash 0 %) (get-data (first args)))))

(defn box-fn [box s]
  (let [[_ code op focal] (first (re-seq #"([a-z]+)(.)([0-9]*)" s))
        box-key (get-hash 0 code)]
    (if (= op "-")
      (update box box-key (fn [coll] (into [] (remove (fn [[ccode _]] (= ccode code)) coll))))
      (update box box-key (fn [coll] (if (some (fn [[ccode _]] (= ccode code)) coll)
                                       (mapv (fn [[ccode n]] (if (= ccode code) [code (parse-long focal) ] [ccode n])) coll)
                                       (conj (or coll []) [code (parse-long focal)])))))))
          

;(box-fn "qp-")

(defn part-2 [& args]
  (let [data (get-data (first args))
        box (reduce box-fn {} data)]
    (reduce
      (fn [aa vi] (+ aa (reduce-kv (fn [a k [_ v]] (+ a (* (inc vi) (inc k) v) )) 0 (get box vi []))))
      0
      (range 256))))


(part-2)
