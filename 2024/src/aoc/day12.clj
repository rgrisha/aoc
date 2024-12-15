(ns aoc.day12
  (:require [clojure.string :as s]
            [aoc.utils      :as u]
            [clojure.set    :as cs]))

(defn line-fn [s]
  (mapv identity s))

(defn get-data [t]
  (let [data (u/get-day-data 12 line-fn t)]
    (reduce-kv (fn [a y line]
                 (reduce-kv (fn [aa x c]
                              (conj aa [x y c]))
                            a
                            line))
               [] data)))


(defn neighbours [[x y c]]
  [[(dec x) y c] [(inc x) y c] [x (dec y) c] [x (inc y) c]])

(defn neighbours-set [[x y c]]
  #{[(dec x) y c] [(inc x) y c] [x (dec y) c] [x (inc y) c]})


(defn cluster-next-region [coords]
  (assert (seq coords) "Coords must be seq")
  (loop [region [] to-check [(first coords)] rem-coords (set (next coords))]
    (if (seq to-check)
      (let [fc (first to-check)
            ngs (neighbours-set fc)
            real-ngs (cs/intersection ngs rem-coords)]
        (recur (conj region fc)
               (into (subvec to-check 1) real-ngs)
               (cs/difference rem-coords real-ngs)))
      [region rem-coords])))

(defn cluster [coords]
  (loop [regions [] cs coords]
    (if (seq cs)
      (let [[region rem-coords] (cluster-next-region cs)]
        (recur (conj regions region) rem-coords))
      regions)))


(defn getc [c data]
  (get data c))

(defn perimeter [region]
  (let [reg-s (into #{} region)
        full-ngs (reduce (fn [rs a]
                           (if (= 4 (count (cs/intersection (neighbours-set a) reg-s)))
                             (conj rs a)
                             rs))
                         #{}
                         region)
        edges (cs/difference reg-s full-ngs)]
    edges))

(defn zzz [reg]
  {:r reg :p (perimeter reg)}) 

;(part-1 :test)
(defn part-1 [& t]
  (let [data (get-data (first t))]
    (->> data
         (group-by (fn [[_ _ c]] c))
         vals
         (reduce (fn [ra cs] (into ra (cluster cs))) [])
         (map zzz))))
         
