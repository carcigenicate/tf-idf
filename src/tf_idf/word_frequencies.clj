(ns tf-idf.word-frequencies
  (:require [net.cgrand.enlive-html :as e]
            [clojure.data.json :as j]
            [clojure.string :as s]
            [helpers.general-helpers :as g]))

(def test-text-path "tiger-text.txt")
(def test-text (delay (slurp test-text-path)))

(defn linkify-text [text]
  (-> text
      (s/replace " " "_")))

(defn query-address [page-name]
  (str "https://en.wikipedia.org/w/api.php?action=query&titles="
       (linkify-text page-name)
       "&format=json&prop=extracts&explaintext&redirects"))

(defn get-page-text
  "Queries Wikipedia and returns a pair of [normalized-name raw-page-text]"
  [page-name]
  (let [raw-json (slurp (query-address page-name))
        parsed (j/read-json raw-json)
        query (:query parsed)

        ; TODO: EWW
        normal-page-name (-> query :normalized (first) :to)
        page-text (-> query :pages (vec) (get-in [0 1]) :extract)]

    [normal-page-name page-text]))

(defn normalize-text [text]
  (let [lower (s/lower-case text)
        cleaned-symbols (s/replace lower #"\-" " ")]
    (apply str
      (filter
        (fn [^Character c]
          (or (Character/isSpace c)
              (Character/isLetter c)))
        cleaned-symbols))))

(defn words [dirty-text]
  (let [clean-text (normalize-text dirty-text)]
    (remove empty?
            (s/split clean-text #"\s"))))

(defn process-page
  "Queries Wikipedia and returns a pair of [normal-page-name word-frequencies]"
  [page-name]
  (let [[normal-name raw-text] (get-page-text page-name)
        page-words (words raw-text)
        words-freqs (frequencies page-words)]

    [normal-name words-freqs]))

(defn words-on-page-for [page-name]
  (let [[_ text] (get-page-text page-name)]
    (words text)))

(defn word-frequencies-for [page-name]
  (frequencies
    (words-on-page-for page-name)))

(defn combine-freq-maps
  "Combines two frequency maps by adding duplicate frequencies together.
  Faster if the bigger map is given as m1."
  [map1 map2]
  (reduce
    (fn [acc [w f]]
      (update acc w #(if %
                       (+ f %)
                       f)))
    map1
    map2))