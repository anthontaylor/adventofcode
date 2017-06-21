(ns neato.shared
  (:require [clojure.java.io :as io]))

(defn parse-dataset
  [name]
  (let [file name]
    (-> file
        (io/resource)
        (io/reader)
        (line-seq))))
