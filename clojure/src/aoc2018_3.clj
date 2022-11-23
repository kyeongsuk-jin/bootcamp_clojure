(ns aoc2018_3
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))


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

(def str-lines (-> "aoc2018-3-1.input"
                   (io/resource)
                   (slurp)
                   (str/split-lines))
  )
; rematched 사용
(defn parse-input [str]
  "입력 받은 문자열을 Map 으로 변경한다.

  ex) (parse-grid-input \"#1 @ 1,3: 4x4\")
  ;=> {:id 1, :start-r 1, :start-c 3, :inc-r 4, :inc-c 4}
  "
  (let [match-format #"^#(\d+)\s@\s(\d+),(\d+):\s(\d+)x(\d+)"
        matched (rest (re-matches match-format str))]
    (when (some? matched)
      (zipmap [:id :start-r :start-c :inc-r :inc-c]
              (->> matched (mapv parse-long)))
      )
    )
  )

(defn init-grid [parsed-input]
  "값을 넣을 격자를 만든다.
  parsed-input -> {:id :start-r :start-c :inc-r :inc-c}

  key -> vector
  value -> set

  ex)
  ;=> {[a b] #{id}}
  "
  (let [{:keys [id start-r start-c inc-r inc-c]} parsed-input]
    (for [row (range start-r (+ start-r inc-r))
          col (range start-c (+ start-c inc-c))]
      {[row col] #{id}}
      )))

(def merged-grid
  (->> str-lines
       (map parse-input)
       (mapcat init-grid)
       (apply merge-with set/union)
       ))

(defn filter-conflicted? [[k v]]
  "{[a b] #{id}}
  id set 이 1보다 큰 지
  "
  (let [c (count v)] (> c 1)))

; {[a b] #{id}} -> set 사이즈가 1보다 크면 count
(->> merged-grid
     (filter filter-conflicted?)
     (vals)
     (count)
     )

;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)

(set/difference (->> merged-grid
                     (vals)
                     (apply set/union))
                (->> merged-grid
                     (filter filter-conflicted?)
                     (vals)
                     (apply set/union)))
