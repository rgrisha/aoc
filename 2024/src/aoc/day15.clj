(ns aoc.day15
  (:require [clojure.string :as s]
            [aoc.utils      :as u]))


(defn to-coords [area]
  (reduce-kv (fn [m y line]
               (reduce-kv (fn [mm x v]
                            (if (= \. v)
                              mm
                              (assoc mm [x y] v)))
                          m
                          (into [] line)))
             {}
             (into [] area)))

(defn get-data [t]
  (let [data (u/get-day-data 15 identity t)
        [area _ moves] (partition-by s/blank? data)]
    [(to-coords area) (apply str moves)]))

(defn separate-robot [area]
  (let [[x y] (some (fn [[ [x y] id] ] (when (= id \@) [x y])) area)]
    [(dissoc area [x y]) [x y]]))

(defn move-left-x [area [rx ry :as robot]]
  (let [fx (dec rx)]
    (loop [lx fx]
      (case (get area [lx ry])
        \# [area robot]
        \O (recur (dec lx))
        nil (if (= lx fx)
              [area [fx ry]]
              [(-> area
                   (dissoc [fx ry])
                   (assoc [lx ry] \O))
               [fx ry]])))))

(defn move [area [rx ry :as robot] next-coord-fn]
  (let [fc (next-coord-fn robot)]
    (loop [nc fc]
      (case (get area nc)
        \# [area robot]
        \O (recur (next-coord-fn nc))
        nil (if (= nc fc)
              [area fc]
              [(-> area
                   (dissoc fc)
                   (assoc nc \O))
               fc])))))

(def coord-fns
  {\< (fn [[x y]] [(dec x) y])
   \> (fn [[x y]] [(inc x) y])
   \^ (fn [[x y]] [x (dec y)])
   \v (fn [[x y]] [x (inc y)])})

(defn draw [area robot]
  (doseq [y (range 0 10)]
    (println (apply str (map (fn [x] (get area [x y] (if (= robot [x y]) \@ \.)))
                             (range 0 10))))))

(defn gps-or-zero [[[x y] b]]
  (if (= \O b)
    (+ x (* 100 y))
    0))


(defn part-1 [& t]
  (let [[coords moves] (get-data (first t))
        [coords robot] (separate-robot coords)
        after (reduce (fn [[a r] m]
                        (when (get a r)
                          (println "error, block at robot position" r))
                        (move a r (get coord-fns m)))
                [coords robot]
                moves)]

    (->> after
         first
         (map gps-or-zero)
         (apply +))))
