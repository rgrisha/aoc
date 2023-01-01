(ns aoc.day14
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure2d.core :as c2d]
            [clojure.core.async :as async :refer [>! <! >!! <!! go chan buffer close! thread
                                                  alts! alts!! timeout]]))

(defn get-data [& ds]
  (let [pairs (u/get-day-data 14 (fn [line]  
                                   (mapv (fn [pair]
                                           (let [[a b] (s/split pair #",")] 
                                             [(parse-long a) (parse-long b)]))
                                         (s/split line #" -> "))) (first ds))]
    pairs))

(defn make-rock [rock pairs]
  (loop [rock rock [px py] (first pairs) pairs (next pairs)]
    (if-not pairs
      rock
      (let [[cx cy] (first pairs)]
        (if (= px cx)
          (recur
            (if (< cy py)
              (reduce (fn [a v] (conj a [cx v])) rock (range cy (inc py)))
              (reduce (fn [a v] (conj a [cx v])) rock (range py (inc cy))))
            (first pairs)
            (next pairs))
          (recur
            (if (< cx px)
              (reduce (fn [a v] (conj a [v cy])) rock (range cx (inc px)))
              (reduce (fn [a v] (conj a [v cy])) rock (range px (inc cx))))
            (first pairs) 
            (next pairs)))))))

(defn create-draw-process [canvas left-bound]
  (let [ch (async/chan)]
    (async/go

      (c2d/with-canvas [c canvas]
        (c2d/scale c 4)
        (c2d/translate c (- 1 (quot left-bound 2)) 1)
        (c2d/set-color c :black)

        (loop []
          (let [[msg-type data] (<! ch)]
            (case msg-type
              :rock (do
                      (c2d/set-color c :black)
                      (doseq [[x y] data]
                        (c2d/rect c x y 1 1))
                      (recur))
              :sand (do
                      ;(println "will draw sand at" data)
                      (c2d/set-color c :yellow)
                      (c2d/rect c (first data) (second data) 1 1)
                      (recur))
              :quit nil)))))
    ch))

(defn is-void-1? [rock sand x y]
  (not (or (contains? rock [x y]) (contains? sand [x y]))))

(defn is-void-2-gen [floor]
  (fn [rock sand x y]
    (not (or (contains? rock [x y])
             (contains? sand [x y])
             (>= y floor)))))

(defn simulate-sand-unit [continue? is-void? rock sand]
  (loop [x 500 y 0]
    (let [next-y (inc y)]
      (when (continue? next-y sand)
        (if (is-void? rock sand x next-y)
          (recur x next-y)
          (cond (is-void? rock sand (dec x) next-y)
                (recur (dec x) next-y)

                (is-void? rock sand (inc x) next-y)
                (recur (inc x) next-y)

                :else
                [x y]))))))


(defn simulate-sand [continue? is-void? rock dp]
  (loop [sand #{}]
    (let [sand-unit (simulate-sand-unit continue? is-void? rock sand)]
      (if sand-unit
        (do
          (>!! dp [:sand sand-unit])
          (recur (conj sand sand-unit)))
        (count sand)))))

(defn run-1 [& ds]
  (let [data (get-data (first ds))
        rock (reduce make-rock #{} data)
        left-bound (reduce (fn [a [x _]] (min a x)) (ffirst rock) rock)
        ;right-bound (reduce (fn [a [x _]] (max a x)) (ffirst rock) rock)
        low-bound (reduce (fn [a [_ y]] (max a y)) (second (first rock)) rock)
        ;pic-data (atom {:rock rock})
        scale 4
        canvas (c2d/canvas 2000 (* scale (+ 3 low-bound)))
        _ (c2d/show-window canvas "Hello World!")
        continue-fn (fn [y _] (<= y low-bound))
        dp (create-draw-process canvas left-bound)]

    (>!! dp [:rock rock])
    (println "result:" (simulate-sand continue-fn is-void-1? rock dp))
    dp))

(defn run-2 [& ds]
  (let [data (get-data (first ds))
        rock (reduce make-rock #{} data)
        left-bound (reduce (fn [a [x _]] (min a x)) (ffirst rock) rock)
        ;right-bound (reduce (fn [a [x _]] (max a x)) (ffirst rock) rock)
        low-bound (reduce (fn [a [_ y]] (max a y)) (second (first rock)) rock)
        ;pic-data (atom {:rock rock})
        scale 4
        canvas (c2d/canvas 2000 (* scale (+ 3 low-bound)))
        _ (c2d/show-window canvas "Hello World!")
        continue-fn (fn [_ sand] (not (and (contains? sand [499 1]) (contains? sand [500 1]) (contains? sand [501 1]))))
        dp (create-draw-process canvas left-bound)
        is-void? (is-void-2-gen (+ low-bound 2))]

    (>!! dp [:rock rock])
    (println "result:" (inc (simulate-sand continue-fn is-void? rock dp)))
    dp))
