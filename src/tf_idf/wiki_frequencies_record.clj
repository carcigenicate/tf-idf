(ns tf-idf.wiki-frequencies-record
  (:require [tf-idf.word-frequencies :as w]))

(def record-dir "./records/")

(def current-record "wikipedia")

(def current-save-path (str record-dir current-record ".txt"))

(defrecord Frequency-Record [total-words word-frequencies])

(defn total-words [frequencies]
  (reduce #(+ % (second %2))
          0
          frequencies))

(defn update-record [freq-record frequencies]
  (let [total-new-words (total-words frequencies)]
    (-> freq-record
        (update :total-words #(+ % total-new-words))
        (update :word-frequencies #(w/combine-freq-maps % frequencies)))))

(defn save-record [freq-record]
  (spit current-save-path
        (pr-str
          (into {} freq-record))))
