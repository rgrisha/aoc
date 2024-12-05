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
    (when-let [not-before-nums (get not-before (nth nums n))]
      (when (contains? not-before-nums (nth nums o))
        n))))



(defn check-after [not-after nums]
  (for [n (range 0 (dec (count nums)))
        o (range n (count nums))]
    (when-let [not-after-nums (get not-after (nth nums n))]
      (when (contains? not-after-nums (nth nums o))
        n))))

(defn pages-ok? [not-before not-after pages-line]
  (and
    (every? nil? (check-before not-before pages-line))
    (every? nil? (check-after not-after pages-line))))

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

(defn gen-sort-fn [not-before not-after]
  (fn [a b]
    (cond
      (when-let [after-nums (get not-before a)]
        (contains? after-nums b))
      -1

      (when-let [before-nums (get not-after b)]
        (contains? before-nums a))
      -1

      (when-let [before-nums (get not-after a)]
        (contains? before-nums b))
      1

      (when-let [after-nums (get not-before b)]
        (contains? after-nums a))
      1

      :else
      0)))


(defn part-2 [& t]
  (let [[_ rules pages] (get-data (first t))
        {:keys [not-before not-after]} (compile-rules rules)
        sort-fn (gen-sort-fn not-before not-after)]
    (->> pages
         (remove (fn [l] (pages-ok? not-before not-after l)))
         (map #(sort sort-fn %))
         (map get-middle)
         (map (fn [s] (Integer. s)))
         (apply +))))

