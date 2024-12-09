(ns aoc.day9
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn line-fn [{:keys [file-id disk]} k v]
  (if (= 0 (mod k 2))
    {:file-id (inc file-id)
     :disk (into disk (replicate v file-id))}
    {:file-id file-id
     :disk (into disk (replicate v nil))}))

(defn get-data [& t]
  (let [line (u/get-day-data 9 identity (first t))]
    (->> line
         first
         (mapv (fn [v] (Integer. (str v))))
         (reduce-kv line-fn {:file-id 0 :disk []}))))

(defn find-first-empty-from [pos data]
  (loop [p pos]
    (if (nil? (nth data p))
      p
      (recur (inc p)))))

(defn find-last-full-backwards-from [pos data]
  (loop [p pos]
    (if (not (nil? (nth data p)))
      p
      (recur (dec p)))))

(defn move-blocks-1 [data]
  (loop [data (transient data)
         empty-pos (find-first-empty-from 0 data)
         full-pos (find-last-full-backwards-from (dec (count data)) data)]
    (let [new-data (-> data (assoc! empty-pos (nth data full-pos))
                            (assoc! full-pos nil))
          next-empty (find-first-empty-from empty-pos data)
          next-full (find-last-full-backwards-from full-pos data)]
      (if (< next-empty next-full)
        (recur new-data next-empty next-full)
        (persistent! data)))))

(defn part-1 [& t]
  (let [{disk :disk} (get-data (first t))
        moved-blocks (move-blocks-1 disk)]
    (apply +
           (map (fn [a b] (* (or a 0) b)) moved-blocks (range)))))


(defn line-fn-2 [{:keys [file-id disk]} k v]
  (if (= 0 (mod k 2))
    {:file-id (inc file-id)
     :disk (conj disk [v file-id])}
    {:file-id file-id
     :disk (conj disk [v nil])}))

(defn get-data-2 [& t]
  (let [line (u/get-day-data 9 identity (first t))]
    (->> line
         first
         (mapv (fn [v] (Integer. (str v))))
         (reduce-kv line-fn-2 {:file-id 0 :disk []}))))

(defn find-prev-full-block [pos data]
  (loop [pos pos]
    (let [[bsize v] (nth data pos)]
      (if v
        pos
        (recur (dec pos))))))

(defn find-first-empty-block [size last-pos data]
  (loop [pos 0]
    (let [[bsize v] (nth data pos)]
      (if (and (nil? v) (>= bsize size))
        pos
        (if (= pos last-pos)
            -1
            (recur (inc pos)))))))


; not proud of this
(defn move-blocks-2 [data]
  (loop [data data last-block (find-prev-full-block (dec (count data)) data)]
    (let [[bsize fv] (nth data last-block)
          epos (find-first-empty-block bsize last-block data)]
      (if (and (= 1 bsize) (> 0 epos))
        data
        (if (> 0 epos)
          (recur data (find-prev-full-block (dec last-block) data))
          (let [[esize _] (nth data epos)]
            (if (= esize bsize)
              (recur
                (-> data
                    (assoc epos [bsize fv])
                    (assoc last-block [bsize nil]))
                (find-prev-full-block (dec last-block) data))
              (let [ddata (assoc data last-block [bsize nil])
                    p1 (subvec ddata 0 epos)
                    p2 (subvec ddata (inc epos))
                    ndata (-> p1
                              (conj [bsize fv])
                              (conj [(- esize bsize) nil])
                              (into p2)
                              (into []))]
                (recur ndata
                       (find-prev-full-block last-block ndata))))))))))



(defn rfn [[pos acc] [cnt v]]
  (if (nil? v)
    [(+ pos cnt) acc]
    [(+ pos cnt)
     (loop [acc acc pos pos cnt cnt]
       (if (= 0 cnt)
         acc
         (recur (+ acc (* pos v)) (inc pos) (dec cnt))))]))


(defn part-2 [& t]
  (let [{disk :disk} (get-data-2 (first t))
        moved-blocks (move-blocks-2 disk)]
    (second
      (reduce rfn [0 0] moved-blocks))))


