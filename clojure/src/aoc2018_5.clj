(ns aoc2018_5
  (:require [clojure.string :as string]))
;; 파트 1
;; 입력: dabAcCaCBAcCcaDA

;; 같은 종류의 소문자와 대문자는 서로 ‘반응‘하여 사라짐. aABb -> ‘’
;; 사라진 자리는 진공이 되기 때문에 다른 문자들이 붙게 되고, 또 그 문자들끼리 반응할 수 있음.  abBA-> aA -> ‘’
;; 바로 옆에 붙어있어야만 서로 반응함. abAB -> abAB (반응 없음)
;; 대문자-대문자, 소문자-소문자는 서로 반응하지 않음. aabAAB-> aabAAB (반응 없음)
;; 예시 dabAcCaCBAcCcaDA => dabCBAcaDA

;; 주어진 input 에서 최종으로 남는 문자열을 리턴하시오.

(def input "dabAcCaCBAcCcaDA")


;; 파트 2
;; 주어진 문자열에서 한 유닛 (대문자와 소문자)을 전부 없앤 후 반응시켰을 때, 가장 짧은 문자열의 길이를 리턴하시오.
;; 예를 들어 dabAcCaCBAcCcaDA 에서 a/A를 없애고 모두 반응시키면 dbCBcD가 되고 길이는 6인데 비해,
;; 같은 문자열에서 c/C를 없애고 모두 반응시키면 daDA가 남고 길이가 4이므로 4가 가장 짧은 길이가 됨.


;;========= stack : PART 1 =========;;
(defn removable?
  [c1 c2]
  (let [required-diff (- (int \a) (int \A))
        diff (abs (- (int c1) (int c2)))]
    (= required-diff diff)))

(defn merge-stack
  [s]
  (reduce
    (fn [r s]
      (let [c (peek r)]
        (if (and (some? c) (removable? c s))
          (pop r)
          (conj r s))))
    [] (seq s)))

(->> (merge-stack input)
     (map str)
     string/join)

;;========= stack : PART 2 =========;;
(defn merge-removed-alphabet
  "입력 받은 char 를 지우고 merge 한다."
  [input char]
  (let [regex (str "(?i)" char)
        pattern (re-pattern regex)
        removed-str (string/replace input pattern "")]
    (merge-stack removed-str)))

(defn reduced-chars
  "입력받은 str 에 대문자 set 을 만들고 각 char 별로 지운 뒤 merge 한다."
  [str]
  (let [chars (->> input string/upper-case distinct vec)]
    (for [char chars] (merge-removed-alphabet str char))))

(->> (reduced-chars input)
     (map count)
     (apply min))

