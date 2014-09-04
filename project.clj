;; Copyright 2014 Positronic Solutions, LLC.
;; All rights reserved.

(defproject com.positronic-solutions/cps "0.1.0-SNAPSHOT"
  :description "CPS transformation macros and trampoline execution engine"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [criterium "0.4.3"]]
  :plugins [[lein-exec "0.3.4"]]
  ;; disable tiered compilation for benchmarks
  :jvm-opts ^:replace [])
