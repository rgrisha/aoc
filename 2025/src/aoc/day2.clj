(ns aoc.day2
  (:require [clojure.string :as s]
            [aoc.utils      :as u]
            [clojure.java.io :as io]))

(defn get-data []
  (with-open [rdr (-> "day-2.txt" io/resource io/reader)]
    (let [l  (.readLine rdr)
          ls (s/split l #",")]
      (mapv (fn [s]
              (let [[a b] (s/split s #"-")]
                [(Long/parseLong a) (Long/parseLong b)]))
            ls))))

(defn has-patterns-twice [n]
  (let [s (str n)
        c (count s)
        h (quot c 2)]
    (if (= 1 (mod c 2))
      false
      (= (.substring s 0 h) (.substring s h)))))

(defn has-repeating-patterns [n]
  (let [s  (str n)
        c  (count s)
        h  (quot c 2)
        gs (for [n (range 1 (inc h))
                 :let [ss (.substring s 0 n)]]
             (apply str (repeat (quot c n) ss)))]
    (if (seq (filter (fn [e] (= e s)) gs))
      true
      false)))

(defn count-for-range [f [fst snd]]
  (apply +
    (for [n (range fst (inc snd))
          :when (f n)]
      n)))

(defn part-1 []
  (let [ps (get-data)
        mf (fn [r] (count-for-range has-patterns-twice r))]
    (apply + (map mf ps))))

(defn part-2 []
  (let [ps (get-data)
        mf (fn [r] (count-for-range has-repeating-patterns r))]
    (apply + (map mf ps))))

