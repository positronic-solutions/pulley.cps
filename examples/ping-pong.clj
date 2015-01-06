;; Copyright 2014 Positronic Solutions, LLC.
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

(require '[com.positronic-solutions.pulley.cps :as cps])

(declare pong)

(def ping
  (cps/cps-fn ([n]
                 (if (> n 0)
                   (do (println "ping!")
                       (pong (dec n)))
                   (println "ping failed to return the volley!")))))

(def pong
  (cps/cps-fn ([n]
                 (if (> n 0)
                   (do (println "pong!")
                       (ping (dec n)))
                   (println "pong failed to return the volley!")))))

(declare cps-odd?)

(def cps-even?
  (cps/cps-fn ([n]
                 (if (> n 0)
                   (cps-odd? (dec n))
                   true))))

(def cps-odd?
  (cps/cps-fn ([n]
                 (if (> n 0)
                   (cps-even? (dec n))
                   false))))
