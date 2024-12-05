(ns aoc.day5
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn line-fn [[st r p :as a] l]
  (cond
    (= "" l) [:pages r p]
    (= :rules st) [st (conj r (s/split l #"\|")) p]
    :else         [st r (conj p (s/split l #","))]))


(defn get-data [t]
  (u/get-day-data-reduce 5 line-fn [:rules [] []] t))

(defn compile-rules [rules]
  {:not-after
     (->> rules
          (group-by second)
          (reduce-kv (fn [m k v] (assoc m k (into #{} (map first v)))) {}))
   :not-before
     (->> rules
          (group-by first)
          (reduce-kv (fn [m k v] (assoc m k (into #{} (map second v)))) {}))})

(defn check-before [not-before nums]
  (for [n (range 1 (count nums))
        o (range 0 n)]
    (let [not-before-nums (get not-before (nth nums n))]
      (if (nil? not-before-nums)
        true
        (not (contains? not-before-nums (nth nums o)))))))


;(let [[_ r p] (get-data :test)
;      {:keys [not-before not-after]} (compile-rules r)))
;  (check-after not-after [ "75" "47" "61" "53" "29"]))

(defn check-after [not-after nums]
  (for [n (range 0 (dec (count nums)))
        o (range n (count nums))]
    (let [not-after-nums (get not-after (nth nums n))]
      (if (nil? not-after-nums)
        true
        (not (contains? not-after-nums (nth nums o)))))))

(defn pages-ok? [not-before not-after pages-line]
  (and
    (every? true? (check-before not-before pages-line))
    (every? true? (check-after not-after pages-line))))

(defn get-middle [pages]
  (nth pages (/ (count pages) 2)))

(defn part-1 [& t]
  (let [[_ rules pages] (get-data (first t))
        {:keys [not-before not-after]} (compile-rules rules)]
    (->> pages
         (filter (fn [l] (pages-ok? not-before not-after l)))
         (map get-middle)
         (map (fn [s] (Integer. s)))
         (apply +))))
