(ns aoc2018_4
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
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
;; 만약 20번 가드가 0시 10분~36분, 다음날 0시 5분~11분, 다다음날 0시 11분~13분 이렇게 잠들어 있었다면, “11분“이 가장 빈번하게 잠들어 있던 ‘분’. 그럼 답은 20 * 11 = 220.


;; 파트 2
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.

(def str-lines (-> "aoc2018-4.input"
                   (io/resource)
                   (slurp)
                   (str/split-lines)))
(defn to-date [str]
  "시간 문자열을 date 형태로 변경한다.
  "
  (let [time-pattern "yyyy-MM-dd hh:mm"
        date-format (SimpleDateFormat. time-pattern)]
    (.setTimeZone date-format (SimpleTimeZone. 0 "UTC"))
    (.parse date-format str)))

(.getMinutes (to-date "1518-11-05 00:55"))

(defn split-time-action [str]
  (let [pattern #"\[(.*)\]\s(.*)"
        [time-str action-str] (rest (re-matches pattern str))
        time (to-date time-str)
        ]
    (when (some? time-str) {:time time :action action-str})))

(defn get-guard [str]
  (let [pattern #"Guard #(\d+) begins shift"
        matched (re-matches pattern str)]
    (when (some? matched) (last matched))
    )
  )

(defn sleep? [str]
  (= "falls asleep" str))

(defn wake-up? [str]
  (= "wakes up" str))

(get-guard "Guard #10 begins shift")

(defn reduce-guard [shift-list]
  (reduce (fn [r x]
            (let [
                  {:keys [time action]} x
                  {:keys [shift-guard sleep-time histories]} r
                  guard-num (get-guard action)
                  slept (sleep? action)
                  woke-up (wake-up? action)
                  action-time (.getMinutes time)]
              (case [(some? guard-num) slept woke-up]
                [true false false] (assoc r :shift-guard guard-num :sleep-time nil)
                [false true false] (assoc r :sleep-time action-time)
                [false false true] (assoc r :sleep-time nil
                                            :histories (merge-with concat {shift-guard [[sleep-time action-time (- action-time sleep-time)]]} histories))
                r
                )
              )
            )
          {:shift-guard 0 :sleep-time nil :histories {}}
          shift-list))

(def merged-guard-histories
  (let [reduced (reduce-guard
                  (->> str-lines
                       (map split-time-action)))]
    (reduced :histories)
    )
  )

merged-guard-histories
; 주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와, 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라
(def guard-id (->> merged-guard-histories
     (map (fn [x] (let [[shift-guard histories] x
                        total-slept (->> histories (last)(apply +))
                        ]
                    {total-slept shift-guard}
                    )))
     (into (sorted-map))
     (last) (last)
))
(->> (merged-guard-histories guard-id)
     (mapcat (fn [x] (let [[start end _] x]
                    (range start (+ end 1))
                    )))
     (frequencies)
     )


