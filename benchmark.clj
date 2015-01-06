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


(require '[com.positronic-solutions.pulley.cps
           :as cps
           :refer [ICallable call cps fn->callable thunk
                   without-recursive-trampolines]])
(require '[criterium.core :as criterium])

;; This example illustrates minimal transformation,
;; with manual priming of the trampoline
(defn factorial-cps1 [n]
  (letfn [(factorial-cont [cont n acc]
            (if (> n 0)
              ;; (factorial-cont (dec n) (* acc n))
              (thunk (factorial-cont cont (dec n) (* acc n)))
              ;; return acc
              (thunk (cont acc))))]
    (cps/trampoline factorial-cont identity n 1)))

;; This example illustrates a moderate degree of transformation,
;; including how letfn could be transformed,
;; and provides for transparent priming of the trampoline
(def factorial-cps2
  (reify
    ICallable
    (with-continuation [self cont env]
      (fn [n]
        ;; letfn shenanigans
        ;; Inspired in part by
        ;; http://cs.brown.edu/courses/cs173/2012/book/Control_Operations.html#(part._.Continuation-.Passing_.Style)
        ((fn [factorial-tco]
           (deliver factorial-tco
                    (reify
                      ICallable
                      (with-continuation [self cont env]
                        (fn [n acc]
                          (if (> n 0)
                            (thunk (call @factorial-tco cont env (dec n) (* acc n)))
                            (thunk (cont acc)))))

                      clojure.lang.IFn
                      (invoke [self n acc]
                        (trampoline self n acc))))
           (thunk (call @factorial-tco cont env n 1)))
         (promise))))

    clojure.lang.IFn
    (invoke [self n]
      (trampoline self n))))

;; Basically the same as factorial-cps2,
;; but simplified using fn->callable
(def factorial-cps3
  (fn->callable (fn [cont env n]
                  ;; letfn shenanigans
                  ;; Inspired in part by
                  ;; http://cs.brown.edu/courses/cs173/2012/book/Control_Operations.html#(part._.Continuation-.Passing_.Style)
                  ((fn [factorial-tco]
                     (deliver factorial-tco
                              (reify
                                ICallable
                                (with-continuation [self cont env]
                                  (fn [n acc]
                                    (if (> n 0)
                                      (thunk (call @factorial-tco cont env (dec n) (* acc n)))
                                      (thunk (cont acc)))))

                                clojure.lang.IFn
                                (invoke [self n acc]
                                  (trampoline self n acc))))
                     (thunk (call @factorial-tco cont env n 1)))
                   (promise)))))

(defn factorial-loop [n]
  (loop [n n
         acc 1]
    (if (> n 0)
      (recur (dec n) (* acc n))
      acc)))

(defn factorial-recur [n]
  (if (> n 0)
    (* n (factorial-recur (dec n)))
    1))

(cps (defn factorial-cps [n]
       (letfn [(factorial-tco [n acc]
                 (if (> n 0)
                   (factorial-tco (dec n) (* n acc))
                   acc))]
         (factorial-tco n 1))))

(cps (defn factorial-recur-cps [n]
       (if (> n 0)
         (* n (factorial-recur-cps (dec n)))
         1)))

(defn benchmark-factorial [n]
  (println "BENCHMARK factorial-recur")
  (criterium/bench (factorial-recur n))

  (println "BENCHMARK factorial-loop")
  (criterium/bench (factorial-loop n))

  #_(println "BENCHMARK factorial-cps1")
  #_(criterium/bench (factorial-cps1 n))

  #_(println "BENCHMARK factorial-cps2")
  #_(criterium/bench (factorial-cps2 n))

  #_(println "BENCHMARK factorial-cps3")
  #_(criterium/bench (factorial-cps3 n))

  (println "BENCHMARK factorial-recur-cps")
  (criterium/bench (factorial-recur-cps n))

  (println "BENCHMARK factorial-cps")
  (criterium/bench (factorial-cps n)))

(defn benchmark-function [f arglists]
  (let [separator (apply str (repeat 40 "="))]
    (doseq [args arglists]
      (println separator)
      (println "args: " args)
      (println separator)

      (apply f args)
      (println))))

(defn run-benchmarks []
  (without-recursive-trampolines
    (benchmark-function benchmark-factorial
                        [[10] [20] [10N] [2543N]])))

(run-benchmarks)
