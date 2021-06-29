(defproject clj-mqtt "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [gloss "0.2.6"]
                 [aleph "0.4.6"]
                 [manifold "0.1.9-alpha4"]
                 [byte-streams "0.2.4"]]
  :main ^:skip-aot clj-mqtt.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
