(ns aoc.day11
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))

(defn line-fn [s]
  (let [ss (s/split s #"[: ]")]
    [(first ss) (into [] (drop 2 ss))]))

(defn get-data [t]
  (into {}
    (u/get-day-data 11 line-fn t)))


(defn calc-to-out [data initial-node next-node-fn merge-next-layer-fn merge-next-layer-initial]
  (loop [layer initial-node]
    (let [next-layer (mapcat
                       (fn [[k v]] (if (= k "out")
                                     [[k v]]
                                     (map (fn [nx] [nx (next-node-fn k v)]) (get data k))))
                       layer)
          non-outs (filter (fn [[k _]] (not= k "out")) next-layer)
          next-layer (group-by first next-layer)
          next-layer (reduce-kv (fn [m k v] (assoc m k (reduce merge-next-layer-fn merge-next-layer-initial (map second v)))) {} next-layer)]
      ;(println "next-layer: " next-layer)
      (if (seq non-outs)
        (recur next-layer)
        next-layer))))


(defn part-1-pm [t]
  (let [net (get-data t)
        ca (calc-to-out net {"you" 1} (fn [_ v] v) + 0)]
    (get ca "out")))


(defn part-1 []
  (part-1-pm :nil))


(defn next-node-fn-2 [pk {:keys [none fft dac both]}]
  (let [[is-fft is-dac none-of] (case pk "fft" [true false false] "dac" [false true false] [false false true])]
    {:none none
     :fft  (if is-fft none fft)
     :dac  (if is-dac none dac)
     :both (+ both (case pk
                     "fft" dac
                     "dac" fft
                     0))}))

(defn merge-next-layer-fn-2 [{nonea :none ffta :fft daca :dac botha :both}
                             {nonee :none ffte :fft dace :dac bothe :both}]
  {:none (+ nonea nonee)
   :fft (+ ffta ffte)
   :dac (+ daca dace)
   :both (+ botha bothe)})

(defn part-2-pm [t]
  (let [net (get-data t)
        ca (calc-to-out net {"svr" {:none 1 :fft 0 :dac 0 :both 0}} next-node-fn-2 merge-next-layer-fn-2 {:none 0 :fft 0 :dac 0 :both 0})
        out (get ca "out")]
    (get out :both)))

(defn part-2 []
  (part-2-pm nil))

; 105917 too low
; 100964926320 too low
