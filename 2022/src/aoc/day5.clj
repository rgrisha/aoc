(ns aoc.day5
  (:require [clojure.string :as s]
            [aoc.utils      :as u]
            [clojure.set    :as cset]
            [clojure.java.io :as io]))

(defn parse-stack-line [stacks line]
  (map-indexed (fn [i st] 
                 (let [line (mapv identity line)
                       pos (inc (* i 4)) 
                       chr (nth line pos)]
                   (if (= chr \space)
                     st 
                     (cons chr st))))
               stacks))

(defn parse-stacks [stack-data]
  (let [stack-data (reverse stack-data)
        stack-count (-> stack-data first s/trim (s/split #"\s+") last parse-long)
        stacks (mapv (constantly '()) (range stack-count))]
    (loop [stacks stacks lines (next stack-data)] 
      (let [line (first lines)]
        (if line
          (recur (parse-stack-line stacks line) (next lines))
          (into [] stacks))))))

(defn parse-instruction [line]
  (mapv (fn [n] (Integer/parseInt n)) (drop 1 (re-matches #"move (\d+) from (\d+) to (\d+)" line))))
  

(defn get-data [] 
  (let [filename "day-5.txt"
        data (slurp (-> filename io/resource io/reader))
        lines (s/split-lines data)
        stacks (take-while #((complement s/blank?)  %) lines)
        instructions (drop (inc (count stacks)) lines)]
    [(parse-stacks stacks) (map parse-instruction instructions)])) 
  
(defn execute-moves [stacks instructions]
  (loop [stacks stacks instructions instructions]
    (let [[m f t] (first instructions)]
      (if (nil? m)
        (apply str (map first stacks))
        (let [fi (dec f)
              ti (dec t)
              c (first (nth stacks fi))
              stacks (-> stacks
                       (update fi (fn [s] (drop 1 s)))
                       (update ti (fn [s] (cons c s))))]
          (if (< m 2)
            (recur stacks (next instructions))
            (recur stacks (cons [(dec m) f t] (next instructions)))))))))

(defn execute-moves-2 [stacks instructions]
  (loop [stacks stacks instructions instructions]
    (let [[m f t] (first instructions)]
      (if (nil? m)
        (apply str (map first stacks))
        (let [fi (dec f)
              ti (dec t)
              cs (take m (nth stacks fi))
              stacks (-> stacks
                       (update fi (fn [s] (drop m s)))
                       (update ti (fn [s] (concat cs s))))]
          (recur stacks (next instructions)))))))
     
(defn run-1 []
  (let [[stacks instructions] (get-data)]
    (execute-moves stacks instructions)))

(defn run-2 []
  (let [[stacks instructions] (get-data)]
    (execute-moves-2 stacks instructions)))
