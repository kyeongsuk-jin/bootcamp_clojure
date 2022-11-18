(ns aoc2018-1
  (:require [clojure.string]))

(def input (slurp "resources/aoc2018-1.input"))
(def str-lines (clojure.string/split-lines input))

;; 파트 1
;; 주어진 입력의 모든 숫자를 더하시오.
;; 예) +10 -2 -5 +1 이 입력일 경우 4를 출력
(->> str-lines
     (map parse-long)
     (reduce +))


;; 파트 2
;; 주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중,
;; 처음으로 두번 나오는 숫자를 리턴하시오.
;; 예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임.
;; 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...

(def numbers (->> str-lines
                  (map parse-long)))

(defn reduce-sum [r x]
  (+ (get r :sum) x)
  )

(defn reduce-second-same-sum [num-arr]
  (reduce
    (fn [r x]
      (let [sum-set (get r :sum-set) sum (reduce-sum r x)]
        (if (sum-set sum)
          (reduced {:result sum})
          {:sum sum :sum-set (conj sum-set sum)})
        )
      )
    {:sum 0 :sum-set #{}}
    num-arr
    )
  )

(defn find-second-same-sum [num-arr]
  (let [result (reduce-second-same-sum num-arr)]
    (get result :result))
  )

(find-second-same-sum numbers)