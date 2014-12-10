(defproject yamlupgrade "0.1.0-SNAPSHOT"
  :description "cassandra.yaml upgrade tool"
  :url "http://github.com/jaimeagudo/yamlupgrade"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [circleci/clj-yaml "0.5.3"]
                 [com.taoensso/timbre "2.7.1"]
                 [diffit "1.0.0"]
                 [org.clojure/tools.cli "0.3.1"]
                 ]
  :main ^:skip-aot yamlupgrade.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
