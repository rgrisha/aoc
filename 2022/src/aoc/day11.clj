(ns aoc.day11
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.java.io :as io]))

(defn parse-operation [sop]
  (let [[a1 op a2] (s/split sop #" ")
        op-fn (get {"+" + "*" *} op)]
    (fn [a]
      (op-fn (if (= a1 "old") a (parse-long a1))
             (if (= a2 "old") a (parse-long a2))))))

(def parse-rules
  [[#"Monkey (\d+).*" (fn [a matches] (assoc a :monkey (Integer/parseInt (second matches))))]
   [#"  Starting items: (.*)" (fn [a matches] (assoc a :items (mapv (fn [s] (parse-long s)) (s/split (second matches) #", "))))]
   [#"  Operation: new = (.*)" (fn [a matches] (assoc a :op (parse-operation (second matches))))]
   [#"  Test: divisible by (\d+)" (fn [a matches] (assoc a :test (parse-long (second matches))))]
   [#"    If true: throw to monkey (\d+)" (fn [a matches] (assoc a :if-true (Integer/parseInt (second matches))))]
   [#"    If false: throw to monkey (\d+)" (fn [a matches] (assoc a :if-false (Integer/parseInt (second matches))))]])
 
(defn parse [d]
  (let [fn-red 
        (fn [a v]
          (reduce (fn [aa [re f]]
                    (if-let [matches (re-matches re v)] 
                      (reduced (f aa matches))
                      aa))
                  a parse-rules))]

    (reduce fn-red {:inspected 0} d)))

(defn get-data [& ds]
  (let [filename (if-let [sfx (first ds)]
                   (str "day-" 11 "-" (name sfx) ".txt")
                   (str "day-" 11 ".txt"))
        data (-> filename io/resource slurp)
        data (s/split data #"\n\n")
        data (mapv s/split-lines data)]
    (mapv parse data)))

(defn update-monkey [monkeys from-monkey monkey-num wl]
  (-> monkeys
      (update-in [monkey-num :items] (fn [a] (conj a wl)))
      (update-in [from-monkey :inspected] inc)))


(defn turn-gen [worry-fn]
  (fn turn [monkey-num monkeys]
    (let [monkey (nth monkeys monkey-num)
          items (:items monkey)]
      (loop [items items monkeys monkeys]
        (if-not (first items)
          (update-in monkeys [monkey-num :items] (constantly []))
          (let [item (first items)
                worry-level ((:op monkey) item)
                worry-level (worry-fn worry-level)]
            (if (= 0 (mod worry-level (:test monkey)))
              (recur (next items) (update-monkey monkeys monkey-num (:if-true monkey) worry-level))
              (recur (next items) (update-monkey monkeys monkey-num (:if-false monkey) worry-level)))))))))
            
(defn round-gen [turn-fn]
  (fn round [monkeys]
    (reduce (fn [a v] (turn-fn v a)) monkeys (range (count monkeys)))))

(defn run-fn [times worry-fn & ds]
  (let [monkeys (get-data (first ds))
        turn-fn (turn-gen worry-fn)
        round-fn (round-gen turn-fn)]
    (->> (iterate round-fn monkeys)
         (drop times)
         first
         (map :inspected)
         (sort #(compare %2 %1))
         (take 2) 
         (apply *))))

(defn run-1 [& ds]
  (run-fn 20 #(quot % 3) (first ds)))

(defn turn-2 [monkey-num monkeys]
  (let [monkey (nth monkeys monkey-num)
        items (:items monkey)]
    (loop [items items monkeys monkeys]
      (if-not (first items)
        (update-in monkeys [monkey-num :items] (constantly []))
        (let [item (first items)
              worry-level ((:op monkey) item)]
          (if (= 0 (mod worry-level (:test monkey)))
            (recur (next items) (update-monkey monkeys monkey-num (:if-true monkey) worry-level))
            (recur (next items) (update-monkey monkeys monkey-num (:if-false monkey) worry-level))))))))


(defn run-2 [& ds]
  (let [monkeys (get-data (first ds))
        mods (mapv :test monkeys)
        round-fn (round-gen turn-2)]
    (->> (iterate round-fn monkeys)
         (drop 10000)
         first
         (map :inspected)
         (sort #(compare %2 %1))
         (take 2) 
         (apply *))))
