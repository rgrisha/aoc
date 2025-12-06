(ns aoc.day6
  (:require [clojure.string :as s]
            [aoc.utils      :as u]
            [clojure.set :as cs]))


(defn get-data [t]
  (let [lines    (u/get-day-data 6 identity t)
        [nn ops] (split-at (dec (count lines)) lines)]
    [nn ops]))

(defn calc-ops [ops]
  (let [line-len (count (first ops))
        ops  (keep-indexed (fn [i e] (when-not (= e \space) [e i])) (first ops))
        opsp (mapv (fn [[o from] [_ to]] [o from to]) ops (rest ops))
        opsp (conj opsp (conj (last ops) (inc line-len)))
        opsp (map (fn [[o a b]] [(get {\* * \+ +} o) a b]) opsp)]
    (into [] opsp)))


(defn calc-for-strings [strs from to mfn finfn]
  (->> strs
       (map (fn [s] (subs s from (dec to))))
       (map mfn)
       finfn))

(defn part-1 []
  (let [[ll ops] (get-data nil)
        cops (calc-ops ops)
        mfn (fn [s] (parse-long (s/trim s)))
        finfn-fn (fn [o] (fn [ll] (reduce o ll)))
        res (map (fn [[o from to]] (calc-for-strings ll from to mfn (finfn-fn o))) cops)]
    (apply + res)))


(defn calc-for-numbers-fn [o]
  (fn [nns]
    (let [ixs (range (count (first nns)))
          vns (mapv (fn [idx]
                      (calc-for-strings nns idx (+ 2 idx) str (fn [ll] (apply str ll))))
                    ixs)
          vns (map (fn [s] (parse-long (s/trim s))) vns)]
      (apply o vns))))

(defn part-2 []
  (let [[ll ops] (get-data nil)
        cops (calc-ops ops)
        res (map (fn [[o from to]] (calc-for-strings ll from to identity (calc-for-numbers-fn o))) cops)]
    (apply + res)))

