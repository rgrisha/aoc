(ns aoc.day4
  (:require [aoc.utils :as u]
            [clojure.string :as s]
            [clojure.set :as cset]))

(defn to-map [line])


(defn read-fn [acc line]
  (let [fi (first acc)]
    (if (empty? line)
      (cons {} acc)
      (cons (into fi (map (fn [l] (s/split l #":")) (s/split line #"\s"))) (rest acc)))))


(def valid-fields-1 #{"ecl" "pid" "eyr" "hcl" "byr" "iyr" "hgt"})

(defn valid-1? [e]
  (let [k (into #{} (keys e))]
    (cset/superset? k valid-fields-1)))

(defn answer-1 [inputs]
  (reduce (fn [a e] (if (valid-1? e) (inc a) a)) 0 inputs ))        

(defn between [e l u]
  (and (>= e l) (<= e u)))

(defn num-between [l u n] 
  (try 
    (between (Integer. n) l u)
    (catch Exception _ false)))

(defn height-valid? [hgt]
  (let [parsed-hgt (re-matches #"(\d+)(cm|in)" hgt)
        dim (nth parsed-hgt 2)  
        vall (nth parsed-hgt 1)]
    (get {"cm" (num-between 150 193 vall)
          "in" (num-between 59 76 vall)} dim false))) 

(def ecls #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(defn ecl-valid? [e] (contains? ecls e) )

(def validations-map 
  {:byr (partial num-between 1920 2002)
   :iyr (partial num-between 2010 2020)
   :eyr (partial num-between 2020 2030)
   :hgt height-valid? 
   :hcl (fn [v] (re-matches #"#[0-9a-f]{6}" v))
   :ecl ecl-valid?
   :pid (fn [v] (re-matches #"\d{9}" v)) })

(defn valid-2? [val-list e]
  (let [ve (into {} e)]
    (every? 
      (fn [[n v]] (doto 
                    (try (v (get ve (name n))) (catch Exception _ false))
                    (println "valid " e n)))
      val-list)))

(defn answer-2 [in-data]
  (let [val-list (into [] validations-map)]
    (->>
      in-data
      (map (partial valid-2? val-list))
      (filter identity)
      count)))
     
(defn run-1-1 []
  (let [in-data (u/get-day-data-reduce 4 read-fn {})]
     (answer-1 in-data)))

(defn run-1-2 []
  (let [in-data (u/get-day-data-reduce 4 read-fn {})]
     (answer-2 in-data)))



 

