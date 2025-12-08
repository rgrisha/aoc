(ns aoc.day7
  (:require [clojure.string :as s]
            [aoc.utils      :as u]
            [clojure.set    :as cs]))


(defn get-data [t]
  (let [lines (u/get-day-data 7 identity t)]
    (keep (fn [e] (when (re-find #"[^\.]" e) e)) lines)))

(defn calc-splits [beams line]
  (keep (fn [b]
            (when (= "^" (subs line b (inc b)))
              [(dec b) (inc b)]))
        beams))

(defn part-1 []
  (let [lines (get-data nil)
        s-pos (s/index-of (first lines) "S")]
    (loop [lines (next lines) splits 0 beams #{s-pos}]
      (if lines
        (let [line     (first lines)
              splits'  (keep (fn [e] (when (= "^" (subs line e (inc e))) e)) beams)]
          (recur (next lines)
                 (+ splits (count splits'))
                 (-> beams
                     (cs/difference splits')
                     (into (mapcat (fn [b] [(dec b) (inc b)]) splits')))))
        splits))))

(defn p2-rfn [acc i]
  (let [v (get acc i)]
    (-> acc
        (update (dec i) + v)
        (update (inc i) + v)
        (assoc i 0))))


(defn part-2 []
  (let [lines (get-data nil)
        acc (mapv (fn [e] (if (= e \S) 1 0)) (first lines))]
    (loop [lines (next lines) acc acc]
      (if lines
        (let [ixs (keep-indexed (fn [i e] (when (= e \^) i)) (first lines))]
          (recur (next lines)
                 (reduce p2-rfn acc ixs)))

        (apply + acc)))))
