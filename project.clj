(defproject com.positronic-solutions.cps "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [criterium "0.4.3"]]
  :plugins [[lein-exec "0.3.4"]]
  ;; disable tiered compilation for benchmarks
  :jvm-opts ^:replace [])
