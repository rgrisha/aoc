(ns aoc.day12
  (:require [aoc.utils :as u]
            [clojure.string :as s]))

(defn nval [c]
  (case c
    \S (int \a)
    \E (int \z)
    (int c)))

(defn line-to-map [[idx m] line]
  (let [cs (seq line)]
    [(inc idx)
     (reduce (fn [a [i c]] (assoc a [idx i] {:code c :val (nval c)}))
             m
             (map-indexed (fn [i c] [i c]) cs))]))

(defn get-data [& ds]
  (let [v (u/get-day-data-reduce 12 line-to-map [0 {}] (first ds))]
    (second v)))

(def nm (for [a [-1 0 1] b [-1 0 1]
              :when (not= [a b] [0 0])]
           [a b]))

(def nm [[0 1] [0 -1] [1 0] [-1 0]])

(defn get-neighbours [data [r c]]
  (for [[a b] nm
        :when (when-let [n (get data [(+ r a) (+ c b)])]
                (let [v (get data [r c])]
                  (>= 1 (- (:val n) (:val v)))))]
    [(+ r a) (+ c b)]))

(defn update-distances [data neighbours dist]
  (println "updating distances " dist " ns" neighbours)
  (reduce (fn [a v]
            (let [{vdist :dist} (get a v)]
              (if (or (nil? vdist)
                      (> vdist (inc dist)))
                (update-in a [v :dist] (constantly (inc dist)))
                a)))
          data
          neighbours))

(defn get-min [data]
  (let [nodes (filter (fn [[_ v]] (and (:dist v) (not (:visited v))) ) data)
        nodes (map first nodes)]
    (println "new nodes to check" nodes)
    (first
      (reduce (fn [[n a] v] (if (< (:dist (get data v)) a)
                              [v (:dist (get data v))]
                              [n a]))
              [(first nodes) (:dist (get data (first nodes)))]
              (rest nodes)))))

(defn calculate-shortest-paths [data]
  (let [start-node (some (fn [[k v]] (when (= (:code v) \S) k)) data)
        data (update-in data [start-node :dist] (constantly 0))]
    (loop [anode start-node data data]
      (let [neighbours (get-neighbours data anode)
            _ (println "active node " anode "nbs" neighbours)
            neighbours (filter (fn [n] (not (:visited (get data n)))) neighbours)
            ndata (update-distances data neighbours (:dist (get data anode)))
            ndata (update-in ndata [anode :visited] (constantly true))
            next-min-node (get-min ndata)]
        (if-not next-min-node
          data
          (recur next-min-node ndata))))))


(defn run-1 [& ds]
  (let [data (get-data (first ds))
        data-with-sp (calculate-shortest-paths data)
        end-node (some (fn [[_ e]] (when (= \E (:code e)) e)) data-with-sp)]

    (:dist end-node)))
