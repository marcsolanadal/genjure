(defproject genjure "0.1.0"
  :description "Genetic Programming Framework for Clojure."
  :url "http://example.com/FIXME"
  :license {:name "MIT License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :main ^:skip-aot genjure.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[midje "1.6.3"]
                                  [criterium "0.4.3"]]}})
