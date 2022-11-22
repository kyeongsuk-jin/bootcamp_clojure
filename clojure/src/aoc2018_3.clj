(ns aoc2018_3
  (:require [clojure.string :as str]
            [clojure.set :as set]))


;; 파트 1
;; 다음과 같은 입력이 주어짐.

;; #1 @ 1,3: 4x4
;; #2 @ 3,1: 4x4
;; #3 @ 5,5: 2x2

;; # 뒤에 오는 숫자는 ID, @ 뒤에 오는 숫자 쌍 (a, b)는 시작 좌표,
;; : 뒤에 오는 (c x d)는 격자를 나타냄.
;; 입력의 정보대로 격자 공간을 채우면 아래와 같이 됨.

;;      ........
;;      ...2222.
;;      ...2222.
;;      .11XX22.
;;      .11XX22.
;;      .111133.
;;      .111133.
;;      ........

;; 여기서 XX는 ID 1, 2, 3의 영역이 두번 이상 겹치는 지역.
;; 겹치는 지역의 갯수를 출력하시오. (위의 예시에서는 4)

(def inputs `("#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2"))

(defn init-grid [start-rows start-cols inc-r inc-c]
  "값을 넣을 격자를 만든다.
  (init-grid 1 3 2 2)
  ex)
  ;=> ([1 3] [2 3] [1 4] [2 4])
  "
  (->> (reduce
         (fn [r row]
           (let [row-range (range start-rows (+ start-rows inc-r))
                 rows (reduce (fn [grid col] (conj grid [col row])) [] row-range)]
             (concat r  rows)))
         []
         (range start-cols (+ start-cols inc-c))
         ))
  )

; 예외 추가 필요 - count 5 가 아닌 경우
(defn parse-grid-input [str]
  "[id a b c d] 형태에 벡터로 만든다."
  (->> (str/split str #"[#@,:x ]+")
       (filterv not-empty)
       (mapv parse-long)
       )
  )

(defn convert-id-map [str]
  "[id (init-grid vectors)] 형태로 만든다.

  ex)
    (
      [1 ([1 3] [2 3] [3 3] [4 3] [1 4] ...)]
      [2 ([3 1] [4 1] [5 1] [6 1] [3 2] ...)]
      [3 ([5 5] [6 5] [5 6] [6 6])]
    )
  "
  (let [[id a b c d] (parse-grid-input str)]
    [id (init-grid a b c d)]
    )
  )

(defn get-key [[row col]]
  "map 을 그리기 위한 key 를 구한다.
  ex) [3 4] -> 34
  "
  (+ (* 10 row) col)
  )

(defn draw-gird [id-grid-map]
  ""
  (let [[id vectors] id-grid-map]
    (reduce
      (fn [r x]
        (assoc r (get-key x) id))
      {}
      vectors
      )))

(defn draw-conflict [v1 v2]
  "겹치는 구역은 X 로 변경"
  (if (= v1 v2)
    v1
    "X"
    )
  )

(->> inputs
     (map convert-id-map) ; [id (init-grid vectors)] 형태로 만든다.
     (map draw-gird) ; id 별로 그린다.
     (apply merge-with draw-conflict) ; merge 하면서 conflict 난 부분을 머지한다.
     (vals)
     (filter (fn [v] (= v "X"))) ; X 만 필터링 한다.
     (count)
     )



;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)

(def drawing-map
  (->> inputs
       (map convert-id-map)
       (map draw-gird)
       ))

(def conflict-grid
  (->> drawing-map
       (map keys)
       (flatten)
       (frequencies)
       (filter (fn [x] (let [[k v] x] (> v 1))))
       (map first)
       (set)
       ))

(defn not-contain-set? [set1 set2]
  (= (count(set/intersection set1 set2)) 0))

(->> drawing-map
  (map (fn [x] (let [[fk fv] (first x)] [fv (into #{} (keys x))])))
  (filter (fn [x] (let [[key girds] x] (not-contain-set? girds conflict-grid))))
  (map first))

