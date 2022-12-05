(ns aoc2018_7
  (:require [aoc2018 :as aoc]
            [clojure.set :as set]
            [clojure.string]))


;Step C must be finished before step A can begin.
;Step C must be finished before step F can begin.
;Step A must be finished before step B can begin.
;Step A must be finished before step D can begin.
;Step B must be finished before step E can begin.
;Step D must be finished before step E can begin.
;Step F must be finished before step E can begin.
;; [C->A]
;; [C->F]
;; [A->B]
;; [A->D]
;; [B->E]
;; [D->E]
;; [F->E]

(def inputs (aoc/read-file "aoc2018-7.input"))

(defn parse-input
  "문자열에 step 을 가져온다."
  [s]
  (let [pattern #"Step (.) must be finished before step (.) can begin."
        [_ f l] (re-matches pattern s)]
    [f l]))

(def steps (->> inputs
                (map parse-input)
                (filter #(some? (first %)))))

(defn map-bfs-data
  "v 를 어떻게 할지 map fn 을 지정한다.

  f - inc or dec 등등."
  [f m k]
  (let [v (m k)] (assoc m k (f v))))

(def linked-map
  "bfs 탐색을 위한 맵을 만든다.
  key - step
  value - [차수 next-steps]
  "
  (let [step-chars (->> steps flatten set)
        default-bfs-map (->> step-chars (map (fn [x] {x 0})) (into {}))
        next-step-map (->> steps (map (fn [[x y]] {x #{y}})) (apply merge-with set/union))]
    (->> steps
         (map last)
         (reduce (fn [r x] (map-bfs-data inc r x)) default-bfs-map)
         (map (fn [[s n]] {s [n (next-step-map s #{})]}))
         (into {}))))

(defn get-bfs-q
  "dfs 차수가 0 인 내역을 가져온다."
  [m]
  (->> m
       (filter (fn [[_ [n _]]] (zero? n)))
       (sort-by first)
       (into {})
       vec))

(defn dec-dfs-count
  "dfs 차수를 출인다 단일 건"
  [m s]
  (let [[c next] (m s)] {s [(dec c) next]}))

(defn dec-dfs-map
  "dfs 차수를 줄인다.
  input m"
  [m next]
  (if (empty? m)
    {}
    (->> next
         (map #(dec-dfs-count m %))
         (into m))))

;; 초기 값
(def start-bfs (get-bfs-q linked-map))
(def initial-data {:dfs-q      start-bfs
                   :result-q   []
                   :linked-map (->> start-bfs (map first) (apply dissoc linked-map))})

(defn sort-bfs
  "bfs 정렬을 시작한다."
  [r _]
  (let [{:keys [dfs-q result-q linked-map]} r
        [step [_ next]] (first dfs-q)
        dec-map (dec-dfs-map linked-map next)
        remain-dfs-q (if (pos? (count dfs-q)) (subvec dfs-q 1) [])
        new-dfs-q (into (get-bfs-q dec-map) remain-dfs-q)]
    {:dfs-q      (->> new-dfs-q (sort-by first) vec)
     :result-q   (conj result-q step)
     :linked-map (->> new-dfs-q (map first) (apply dissoc dec-map))}))

;; 결과
(->> (cycle [0])
     (reductions sort-bfs initial-data)
     (reduce (fn [r x] (let [{:keys [dfs-q result-q]} r]
                         (if (empty? dfs-q)
                           (reduced result-q)
                           x))) initial-data)
     (apply str))

