(ns aoc2018_6
  (:require [aoc2018 :as aoc]
            [clojure.string]))

;; 파트 1
;; 입력 : 좌표의 쌍이 N개 주어짐

;; 1, 1
;; 1, 6
;; 8, 3
;; 3, 4
;; 5, 5
;; 8, 9

;; 각 점은 1 tick이 지날때 마다 상,하,좌,우로 증식함.

;;  ..........
;;  .A........
;;  ..........
;;  ........C.
;;  ...D......
;;  .....E....
;;  .B........
;;  ..........
;;  ..........
;;  ........F.

;;  aaaaa.cccc
;;  aAaaa.cccc
;;  aaaddecccc
;;  aadddeccCc
;;  ..dDdeeccc
;;  bb.deEeecc
;;  bBb.eeee..
;;  bbb.eeefff
;;  bbb.eeffff
;;  bbb.ffffFf


;; 여기서 . 으로 표기된 부분은 각 출발 지점으로부터 '같은 거리'에 있는 부분을 뜻함.
;; 맵 크기에는 제한이 없어 무한으로 뻗어나간다고 할 때, 가장 큰 유한한 면적의 크기를 반환 (part-1)

(def inputs (->> (aoc/read-file "aoc2018-6.input")
                 (map #(clojure.string/split % #", "))
                 (map (fn [[x y]] [(parse-long x) (parse-long y)]))))

(defn get-char
  "입력 받은 수량 만큼의 알파벳을 리턴한다."
  [len]
  (let [start (int \a)]
    (->> (range start (+ start len))
         (map char)
         (map str))))

(defn create-start-areas
  "
  { [1 1] a,
    [1 6] b,
    [8 3] c,
    [3 4] d,
    [5 5] e,
    [8 9] f }
 "
  [areas]
  (into {} (map vector areas (get-char (count areas)))))

(defn init-grid
  "A-F 를 그릴수 있는 grid 를 만든다.

  ex) list ({[x y] nil}
  "
  [arr]
  (let [x-arr (map first arr)
        y-arr (map last arr)
        x-min (apply min x-arr)
        x-max (apply max x-arr)
        y-min (apply min y-arr)
        y-max (apply max y-arr)]
    (into {}
          (for [x (range x-min (inc x-max))
                y (range y-min (inc y-max))]
            {[x y] nil}))))

(defn init-grid-map
  "처음 시작 map 설정.
  m - {:grid-map :remain-grid }
  "
  [areas]
  (let [remain-grid (init-grid areas)
        inc-map (create-start-areas areas)]
    {:grid-map    inc-map
     :remain-grid (->> inc-map (map first) (apply dissoc remain-grid))}))

(defn inc-areas
  "m - {:grid-map :remain-grid} 로 다음 증가하는 위치 정보 map 을 구한다.

  remain-grid 를 기준으로 인접한 4곳의 현재 상태를 보고 다음 상태를 예측한다.
  "
  [m]
  (reduce
    (fn [r pivot]
      (let [{:keys [grid-map]} m
            [k _] pivot
            [x y] k
            directions [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]
            vals (->> directions (map grid-map) (filter some?) (set))]
        (cond
          (empty? vals) r
          (= 1 (count vals)) (into r {k (first vals)})
          :else (into r {k "."}))))
    {} (m :remain-grid)))


(defn reduce-inc [m]
  (reduce (fn [r _]
            (let [{:keys [grid-map remain-grid]} r
                  areas (inc-areas r)]
              (if (empty? areas)
                (reduced r)
                {:grid-map    (into grid-map areas)
                 :remain-grid (->> areas (map first) (apply dissoc remain-grid))})))
          m
          (range 0 (count (m :remain-grid)))))

(defn find-finite-chars
  "유한한 지점 문자 를 찾는다."
  [m]
  (let [areas (->> m (map first))
        x-min (->> areas (map first) (apply min))
        x-max (->> areas (map first) (apply max))
        y-min (->> areas (map last) (apply min))
        y-max (->> areas (map last) (apply max))]
    (->> (for [[[x y] ch] m]
           (when (and (> x x-min) (< x x-max) (> y y-min) (< y y-max)) ch))
         (filter some?) set)))

(def init-map (init-grid-map inputs))
(def finite-chars (find-finite-chars (init-map :grid-map)))

(apply max (map (-> init-map
                    reduce-inc
                    :grid-map
                    vals
                    frequencies) finite-chars))





;; 파트 2
;; 안전(safe) 한 지역은 근원지'들'로부터의 맨하탄거리(Manhattan distance, 격자를 상하좌우로만 움직일때의 최단 거리)의 '합'이 N 미만인 지역임.

;;  ..........
;;  .A........
;;  ..........
;;  ...###..C.
;;  ..#D###...
;;  ..###E#...
;;  .B.###....
;;  ..........
;;  ..........
;;  ........F.

;; Distance to coordinate A: abs(4-1) + abs(3-1) =  5
;; Distance to coordinate B: abs(4-1) + abs(3-6) =  6
;; Distance to coordinate C: abs(4-8) + abs(3-3) =  4
;; Distance to coordinate D: abs(4-3) + abs(3-4) =  2
;; Distance to coordinate E: abs(4-5) + abs(3-5) =  3
;; Distance to coordinate F: abs(4-8) + abs(3-9) = 10
;; Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30

;; N이 10000 미만인 안전한 지역의 사이즈를 구하시오.

(defn distance [[x1 y1] [x2 y2]]
  (let [x (abs (- x1 x2))
        y (abs (- y1 y2))]
    (+ x y)))

(defn sumDistance
  [point areas]
  (apply + (for [area areas]
             (->> (distance point area)))))


(defn part2
  [n areas]
  (->> (reductions + (cycle [1]))
       (reduce (fn [r x]

                 (if (> x 100)
                   (reduced x)
                   x)) 0)))

(part2 100 inputs)
