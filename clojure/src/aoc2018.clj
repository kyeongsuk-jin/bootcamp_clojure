(ns aoc2018
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-file [file-name]
  (-> file-name
      (io/resource)
      (slurp)
      (str/split-lines)))