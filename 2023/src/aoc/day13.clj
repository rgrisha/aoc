(ns aoc.day13
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn get-data [ & params] 
  (->>  (u/get-day-data 13 identity (first params))
        (partition-by #{""})
        (filter #(> (count %) 1))
        (mapv (fn [m] (into [] m)))))

(defn strings-of-cols [smap]
  (let [length (count (first smap))]
    (loop [i 0 acc []]
      (if (< i length)
        (recur (inc i) (conj acc (apply str (map #(nth % i) smap))))
        acc))))

(defn find-reflection-old [smap]
  (let [last  (dec (count smap))
        gp    (group-by second (map-indexed (fn [i e] [i e]) smap))
        vs    (vals gp) 
        idxs  (mapv (fn [e] (mapv first e)) vs)
        idxs  (filterv #(< 1 (count %)) idxs)
        idxss (mapv set idxs)
        outer-edge (first (keep (fn [l] (when (some #{0 last} l) l) ) idxs))
        _ (println idxs idxss outer-edge)]
    ;check if outer edge leads until the center
    (when-let [[upper lower] outer-edge]
      (loop [upper (inc upper) lower (dec lower)]
        (when (and (< upper lower) 
                   (some (fn [s] (and (contains? s upper) (contains? s lower))) idxss))
          (if (= 1 (- lower upper))
            lower
            (recur (inc upper) (dec lower))))))))

(defn find-reflection-cmp [sm1 sm2]
  (loop [deleted 0 m2 sm2]
    (when (> (count m2) 1)
      (if (every? true? (map (fn [a b] (= a b)) sm1 m2))
        [deleted  (bit-shift-right (count m2) 1)]
        (recur (inc deleted) (next m2))))))
         

(defn find-reflection [smap]
  (let [[_ n] (find-reflection-cmp smap (reverse smap))]
    (if n
      n
      (let [[d n] (find-reflection-cmp (reverse smap) smap)]
        (when n
          (+ d n))))))

(defn part-1 [& args]
  (let [maps (get-data (first args))]
    (reduce-kv
      (fn [acc idx imap]
        (println "searching map " idx imap)
        (if-let [res (find-reflection imap)]
          (+ acc (* 100 res))
          (let [res (find-reflection (strings-of-cols imap))]
            (+ acc res))))
      0
      maps)))

;     abcddcbafd
;   dfabcddcba

;

