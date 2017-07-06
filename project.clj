(defproject neato "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha16"]
                 [org.clojure/tools.trace "0.7.9"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [aprint "0.1.3"]
                 [philoskim/debux "0.3.4"]
                 [walmartlabs/datascope "0.1.1"]]
  :main ^:skip-aot neato.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
