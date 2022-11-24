(ns aoc2018_4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.spec.alpha :as s])
  (:import (java.text SimpleDateFormat)
           (java.util SimpleTimeZone)))

(use 'flatland.ordered.map)
;; 파트 1
;; 입력:

;; [1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up
;; [1518-11-01 00:30] falls asleep
;; [1518-11-01 00:55] wakes up
;; [1518-11-01 23:58] Guard #99 begins shift
;; [1518-11-02 00:40] falls asleep
;; [1518-11-02 00:50] wakes up
;; [1518-11-03 00:05] Guard #10 begins shift
;; [1518-11-03 00:24] falls asleep
;; [1518-11-03 00:29] wakes up
;; [1518-11-04 00:02] Guard #99 begins shift
;; [1518-11-04 00:36] falls asleep
;; [1518-11-04 00:46] wakes up
;; [1518-11-05 00:03] Guard #99 begins shift
;; [1518-11-05 00:45] falls asleep
;; [1518-11-05 00:55] wakes up

;; 키워드: 가드(Guard) 번호, 자는 시간(falls asleep), 일어나는 시간(wakes up).
;; 각 가드들은 교대 근무를 시작하고 (begins shift) 졸았다가 일어났다를 반복함.
;; 위의 예시에서 10번 가드는 0시 5분에 잤다가 25분에 일어나고, 또 0시 30분에 잠들었다가 0시 55분에 깨어남.
;; 가드들에 대해서 자고 깨는 시간 정보들이 입력으로 주어짐.

;; 파트 1은 “주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와, 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라”
;; 만약 20번 가드가 0시 10분~36분, 다음날 0시 5분~12분, 다다음날 0시 11분~13분 이렇게 잠들어 있었다면, “11분“이 가장 빈번하게 잠들어 있던 ‘분’. 그럼 답은 20 * 11 = 220.
;;

;; 파트 2
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.

(def str-lines (-> "aoc2018-4.input"
                   (io/resource)
                   (slurp)
                   (str/split-lines)))

; java-time
(defn to-date [str]
  "시간 문자열을 date 형태로 변경한다.
  "
  (let [time-pattern "yyyy-MM-dd hh:mm"
        date-format (SimpleDateFormat. time-pattern)]
    (.setTimeZone date-format (SimpleTimeZone. 0 "UTC"))
    (.parse date-format str)))

(defn get-guard-id [str]
  "
  guard-id 정보를 가져온다.
  "
  (let [pattern #"Guard #(\d+) begins shift"
        [_ matched] (re-matches pattern str)]
    (when (some? matched) (parse-long matched))))

(defn parse-guard-history [str]
  "
  근무이력을 map 으로 변경한다.
  key - :GUARD|:SLEEP|:WAKE
  val - guard-id or minutes

  ex)

  ([:GUARD 10] [:SLEEP 5] [:WAKE 25] [:SLEEP 30] [:WAKE 55])
  "
  (let [pattern #"\[(.*)\]\s(.*)"
        [_ time-str action-str] (re-matches pattern str)
        time (to-date time-str)
        minutes (.getMinutes time)
        guard-id (get-guard-id action-str)]
    (if (some? guard-id)
      [:GUARD guard-id]
      (case [action-str]
        ["falls asleep"] [:SLEEP minutes]
        ["wakes up"] [:WAKE minutes]
        {}
        ))))

(defn merge-guard-histories [guard-histories]
  "
  가드별 근무 이력을 merge 한다.
  key - guard-id
  value - merged histories ([sleep-minute wake-minute])
  "
  (reduce (fn [r x]
             (let [[action value] x
                   {:keys [shift-guard sleep-minute histories]} r]
               (case action
                 :GUARD (assoc r :shift-guard value :sleep-minute nil)
                 :SLEEP (assoc r :sleep-minute value)
                 :WAKE (assoc r :sleep-minute nil
                                :histories
                                (merge-with concat {shift-guard [[sleep-minute value]]} histories))
                 r
                 )))
          {:shift-guard nil :sleep-minute nil :histories {}}
          guard-histories))

(def guard-slept-map
  "가드가 잤던 분 range 에 대한 map 을 만든다.
  ;key guard-id
  ;value list
  ex)
  ; => {99 ([45 55] [36 46] [40 50]), 10 ([24 29] [30 55] [5 25])}
  "
  (->> str-lines
       (map parse-guard-history)
       (merge-guard-histories)
       (:histories)))

;===============================================================
; 주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와,
; 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라

(defn merge-slept-minute [guard-entry-map]
  "guard 의 총 sleep 시간을 머지 한다.
  "
  (let [[shift-guard histories] guard-entry-map
        total-slept (->> histories
                         (reduce (fn [r x] (let [[start end] x] (+ r (- end start)))) 0))]
    [shift-guard total-slept]))

(defn multiply-most-often-slept-guard-with-minute [guard-slept-map]
  "가장 많이 잔 guard id 와 가장 자주 잠드는 분을 곱한다."
  (let [max-slept-guard-id
        (->> guard-slept-map
          (map merge-slept-minute)
          (reduce (fn [r x]
                    (let [[_ slept-minutes] x [_ max-slept] r]
                      (if (> slept-minutes max-slept) x r))) [nil 0])
          (first))
        max-slept-minute
        (->> (guard-slept-map max-slept-guard-id)
           (mapcat (fn [x] (let [[start end _] x] (range start end))))
           (frequencies)
           (reduce (fn [r x]
                     (let [[_ target] x
                           [_ max] r]
                       (if (> target max) x r)))[nil 0])
           (first))]
    (* max-slept-guard-id max-slept-minute)
    ))

(multiply-most-often-slept-guard-with-minute guard-slept-map)


;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.
(defn count-in-range [minute histories]
  "
   minute 이 속하는 range 갯수를 더한다.
  "
  (reduce (fn [r x]
            (let [[start end] x]
              (if (s/int-in-range? start end minute)
                (+ r 1) r))) 0 histories))

(defn convert-range-minute-count [minute guard-slept-map]
  "
  [99 ([45 55] [36 46] [40 50]), 10 ([24 29] [30 55] [5 25])]

  45분이면
  -> ([99 3] [10 1])
  "
  (reduce (fn [r x]
            (let [{:keys [count]} r
                  [x-guard-id ranges] x
                  x-count (count-in-range minute ranges)]
            (if (> x-count count)
              {:guard-id x-guard-id :count x-count}
              r)))
            {:guard-id nil :count 0}
            guard-slept-map)
  )

(defn multiply-most-often-guard-with-minute [minute guard-slept-map]
  "값을 곱한다."
  (let [guard-id (->> guard-slept-map
                      (map vec)
                      (convert-range-minute-count minute)
                      (:guard-id)
                      )
        ]
    (* minute guard-id)
    )
  )

(multiply-most-often-guard-with-minute 45 guard-slept-map)