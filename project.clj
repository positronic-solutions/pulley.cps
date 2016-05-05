;; Copyright 2014-2015 Positronic Solutions, LLC.
;;
;; This file is part of pulley.cps.
;;
;; pulley.cps is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; pulley.cps is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with pulley.cps.  If not, see <http://www.gnu.org/licenses/>.

(defproject com.positronic-solutions/pulley.cps "0.2.2-SNAPSHOT"
  :description "A macro-based CPS transformation compiler and runtime library"
  :url "https://github.com/positronic-solutions/pulley.cps"
  :license {:name "GNU Lesser General Public License, version 3 or later"
            :url "http://www.gnu.org/licenses/lgpl.html"
            :distribution :repo}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [criterium "0.4.3"]]
  :plugins [[lein-exec "0.3.4"]]
  ;; disable tiered compilation for benchmarks
  :jvm-opts ^:replace [])
