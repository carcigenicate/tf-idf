(defproject tf-idf "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [helpers "1"]
                 [enlive "1.1.6"]
                 [org.clojure/data.json "0.2.6"]]
  :main tf-idf.main
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
