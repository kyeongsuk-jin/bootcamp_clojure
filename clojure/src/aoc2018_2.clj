(ns aoc2018-2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def str-lines (-> "aoc2018-2.input"
                   (io/resource)
                   (slurp)
                   (str/split-lines)))

;; 파트 1
;; 주어진 각각의 문자열에서, 같은 문자가 두번 혹은 세번씩 나타난다면 각각을 한번씩 센다.
;; 두번 나타난 문자가 있는 문자열의 수 * 세번 나타난 문자가 있는 문자열의 수를 반환하시오.
;; 예)
;; abcdef 어떤 문자도 두번 혹은 세번 나타나지 않음 -> (두번 나오는 문자열 수: 0, 세번 나오는 문자열 수: 0)
;; bababc 2개의 a, 3개의 b -> (두번 나오는 문자열 수: 1, 세번 나오는 문자열 수: 1)
;; abbcde 2개의 b -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 1)
;; abcccd 3개의 c -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 2)
;; aabcdd 2개의 a, 2개의 d 이지만, 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 3, 세번 나오는 문자열 수: 2)
;; abcdee 2개의 e -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 2)
;; ababab 3개의 a, 3개의 b 지만 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 3)
;; 답 : 4 * 3 = 12

(comment
  (def input-str "abcedbb")

  (-> input-str
      (char-array)                                          ; (\a \b ...)
      (seq)
      (frequencies)
      (vals)
      (set)
      #_(seq)
      #_(frequencies))
  )




(defn char-count-set [str]
  (-> str
      (frequencies)
      (vals)
      (set)
      )
  )

(defn contains-two-three? [x]
  (#{2 3} x))

(->> str-lines
     (map char-count-set)
     (apply concat)
     (filter contains-two-three?)
     (frequencies)  ;; 함수로 한번 만들어보자.
     (vals)
     (apply *)
     )


;; 파트 2
;; 여러개의 문자열 중, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍에서 같은 부분만을 리턴하시오.
;; 예)
;; abcde
;; fghij
;; klmno
;; pqrst
;; fguij
;; axcye
;; wvxyz

;; 주어진 예시에서 fguij와 fghij는 같은 위치 (2번째 인덱스)에 정확히 한 문자 (u와 h)가 다름. 따라서 같은 부분인 fgij를 리턴하면 됨.
(def part2-arr ["abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz" "xvxyz"])
;;["abcde" "fghij"]
;; => [["a" "f"] ["b" "g"] ]
;; => (map vector c1 c2)
;
(defn check-same-char? [source target index]
  (= (get source index) (get target index))
  )
;
(defn len-str [s t]
  (max (count s) (count t))
  )
;

(defn compare-str [str1 str2]
  (reduce
    (fn [r i]
      ; https://clojure.org/guides/destructuring#_associative_destructuring
      (let [source (r :source)
            target (r :target)
            same-str (r :same-str)
            length (len-str source target)]
        (if (check-same-char? source target i)
          {:same-str (str same-str (get source i))
           :source source
           :target target
           :length length
           } ; assoc / update
          ; (assoc r :same-str new-val)
          ; (update r :same-str new-fn)
           r)))
    {:same-str "" :length 0 :source str1 :target str2}
    (range (len-str str1 str2))))

(defn compare-arr [arr str]
  (reduce
    (fn [r x]
      (conj r (compare-str str x))
      )
    []
    arr)
  )

(defn reduce-compare
  "reduce compare
  ex)
  input) 1
  output) 2
  "
  [arr index-arr]
  (reduce
    (fn [r x]
      (let [target (arr x)]
        (conj r (compare-arr (subvec arr x) target))
        ))
    []
    index-arr
    )
  )

(defn filter-one-diff?
  [r]
  (= (- (r :length) 1) (count (r :same-str)))
  )

; 리펙토링 전
(->> (range 0 (- (count part2-arr) 1)) ;
     (reduce-compare part2-arr) ;
     (flatten) ;
     (filter filter-one-diff?) ;
     (map :same-str)
     )

;; PPAP
;; parse
;; process
;; aggregate
;; print

;; #################################
;; ###        Refactoring        ###
;; #################################
