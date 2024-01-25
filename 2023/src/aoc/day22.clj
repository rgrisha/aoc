(ns aoc.day22
  (:require [clojure.set    :as cs]
            [clojure.string :as s]
            [aoc.utils      :as u]))

(defn parse-line [s]
  (mapv (fn [br] (mapv parse-long (s/split br #","))) (s/split s #"~")))

(defn get-data [& args] 
  (sort-by
    (fn [[[_ _ z] [_ _ _]]] z)
    (u/get-day-data 22 parse-line (first args))))

(defn get-brick-body [[[x1 y1 z1] [x2 y2 z2]]]
  (cond 
    (not= x1 x2)
    (mapv (fn [x] [x y1 z1]) (range x1 (inc x2)))
  
    (not= y1 y2)
    (mapv (fn [y] [x1 y z1]) (range y1 (inc y2)))

    :else
    (mapv (fn [z] [x1 y1 z]) (range z1 (inc z2)))))

;(mapv get-brick-body (get-data :test))
;(get-brick-body [[1 0 1] [1 0 1]])


(defn just-under? [[[_ _ _] [_ _ z1]] [[_ _ z2] [_ _ _]]]
  (= (inc z1) z2))

(defn put-brick-on-top-1 [[[x1 y1 z1] [x2 y2 z2] :as brick] pile-layer structure]
  (let [brick-body    (get-brick-body brick)
        body-xy       (into #{} (map (fn [[x y _]] [x y]) brick-body))
        max-z-under   (apply max (map (fn [a] (first (get pile-layer a [0]))) body-xy))
        under-bricks  (into #{} (remove nil? (map (fn [a] (second (get pile-layer a ))) body-xy)))
        diff-z        (- z1 max-z-under)
        new-top       (inc (- z2 diff-z))
        ;_ (println "df" diff-z "nt" new-top)
        new-brick     [[x1 y1 (inc (- z1 diff-z))] [x2 y2 new-top]]
        new-layer     (reduce (fn [a v] (assoc a v [new-top new-brick])) pile-layer body-xy)
        under-bricks  (filter (fn [ub] (just-under? ub new-brick)) under-bricks)
        new-structure (assoc structure new-brick under-bricks)]
    [new-brick new-layer new-structure]))

      
(defn stack-all-bricks [bricks]
  (loop [undone-bricks bricks done-bricks [] pile-layer {} structure {}]
    (if (seq undone-bricks)
      (let [brick (first undone-bricks)
            ;_ (println "before put:" brick pile-layer)
            [brick pile-layer structure] (put-brick-on-top-1 brick pile-layer structure)]
            ;_ (println "after put:" brick pile-layer)]
        (recur (next undone-bricks) (conj done-bricks brick) pile-layer structure))
      [done-bricks structure])))

(defn get-number-of-paired-bricks [structure]
  (let [vs (vals structure)
        single-keepers (into #{} (filter (fn [bs] (= 1 (count bs))) vs))]
    (- (count structure) (count single-keepers))))



;517
(defn part-1 [& args]
  (get-number-of-paired-bricks 
    (second 
      (stack-all-bricks 
        (get-data (first args))))))


(defn stack-with-sets [stack]
  (reduce-kv (fn [a k v] (assoc a k (into #{} (if (seq v) v [[]])))) {} stack))

(defn count-fallen-bricks [stack brick]
  (loop [fallen #{brick} stack (stack-with-sets (dissoc stack brick))]
    (let [fallen-in-round (keep (fn [[k v]] 
                                  (when (empty? (cs/difference v fallen)) 
                                        k)) 
                                stack)]
      (if (seq fallen-in-round)
        (recur (into fallen fallen-in-round) (apply dissoc stack fallen-in-round)) 
        (dec (count fallen))))))


(defn part-2 [& args]
  (let [[_ stack] (stack-all-bricks (get-data (first args)))]
    (reduce (fn [a v] (+ a (count-fallen-bricks stack v))) 0 (keys stack))))
