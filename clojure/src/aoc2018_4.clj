(ns aoc2018_4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [java-time :as jt]
            [aoc2018 :as aoc])
  (:import (java.text SimpleDateFormat)
           (java.util SimpleTimeZone)))
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
(def str-lines (aoc/read-file "aoc2018-4.input"))

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

  number or nil
  "
  (let [pattern #"Guard #(\d+) begins shift"
        [_ matched] (re-matches pattern str)]
    (when (some? matched) (parse-long matched))))

(defn reduce-parse-guard-history
  "parse 후에 바로 reduce 로 {id [total-minute range-frequencies]}"
  [guard-histories]
  (-> (reduce
        (fn [r str]
          (let [pattern #"\[(.*)\]\s(.*)"
                [_ time-str action-str] (re-matches pattern str)
                guard-id (get-guard-id action-str)]
            (if (some? guard-id)
              (assoc r :shift-guard guard-id)
              (let [time (to-date time-str)
                    minute (.getMinutes time)]
              (case [action-str]
                ["falls asleep"] (assoc r :sleep-minute minute)
                ["wakes up"]
                (let [{:keys [shift-guard sleep-minute histories]} r
                      m {shift-guard [[sleep-minute minute]]}]
                (assoc r :histories (merge-with concat m histories)))
                r)))))
        {:shift-guard nil :sleep-minute nil :histories {}}
        guard-histories)
      (:histories))
  )

(defn get-total-slept
  "총 잠든 시간을 구한다."
  [slept-range]
  (->> slept-range
       (reduce
         (fn [r x]
           (let [[start end] x]
             (+ r (- end start))))
         0)))

(defn get-minute-count-map
  "minute frequencies map"
  [slept-ranges]
  (->>
    slept-ranges
    (mapcat (fn [x] (let [[start end] x] (range start end))))
    (frequencies)
    )
  )

(def guard-slept-map
  "map 을 만든다.

  {:guard-id :total-slept :frequencies}
  ex)
  ; => {99 ([45 55] [36 46] [40 50]), 10 ([24 29] [30 55] [5 25])}
  "
  (->> str-lines
       (reduce-parse-guard-history)
       (map
         (fn [x]
           (let [[guard-id slept-ranges] x
                 total-slept (get-total-slept slept-ranges)
                 minute-count (get-minute-count-map slept-ranges)]
             {:guard-id guard-id :total-slept total-slept :minute-count minute-count}
             )))))


;===============================================================
; 주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와,
; 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라
(def get-max-slept-guard
  (last (sort-by :total-slept guard-slept-map)))
(defn get-max-minute [guard]
  (let [{:keys[minute-count]} guard]
    (->> minute-count
         (sort-by val)
         last key
         )
    ))

(->> get-max-slept-guard
     ((juxt :guard-id get-max-minute))
     (apply *))

;=======================================================================
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.
guard-slept-map
(defn part2
  [m minute]
  (* minute (->> m
       (reduce
         (fn [r x]
           (let [[_ max-count] r
                 {:keys [guard-id minute-count]} x
                 count (minute-count minute 0)]
             (if (> count max-count)
               [guard-id count]
               r
               )
             )) [0 0])
       (first)))
  )
(part2 guard-slept-map 45)