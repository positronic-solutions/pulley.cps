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

(ns com.positronic-solutions.pulley.cps-test
  (:use clojure.test
        [clojure.pprint :only [pprint]]
        com.positronic-solutions.pulley.cps))

(def ^:dynamic *foo* 0)
(def ^:dynamic *bar* 0)

(defmacro verify-form-equiv
  "Macro for testing that a given input form evaluates to the same value,
regardless of whether it is passed through the CPS compiler.

Basically, two copies of the form will be created.
One will be passed through the CPS compiler, the other will not.
Both forms will then be evaluated and the resulting values compared
to ensure they are equivalent."
  ([form]
     `(testing (with-out-str (pprint ~form))
        (is (= ~form
               (cps ~form))))))

(deftest test-atomic-expression
  (without-recursive-trampolines
   (with-strict-cps
     (verify-form-equiv 1)
     (verify-form-equiv "Hello, world!")
     (let [x 1]
       (verify-form-equiv x)))))

(deftest test-binding
  (without-recursive-trampolines
   (with-strict-cps
     (verify-form-equiv (binding [*foo* 2]
                          *foo*))
     (verify-form-equiv (do (binding [*foo* 2])
                            *foo*)))
   (verify-form-equiv (binding [*foo* 2
                                *bar* 5]
                        (* *foo* *bar*)))
   (verify-form-equiv (+ (binding [*foo* 2
                                   *bar* (+ *foo* 5)]
                           (* *foo* *bar*))
                         (- 1 *foo* *bar*)))
   (testing "let-shadowed binding"
     (with-strict-cps (let [*foo* 10]
                        (binding [*foo* 20]
                          *foo*))))
   (testing "cps->non-cps"
     (letfn [(foo-value []
               *foo*)]
       (verify-form-equiv (binding [*foo* 20]
                            (foo-value)))))
   (testing "non-cps->cps"
     (binding [*foo* 20]
       (with-strict-cps
         (verify-form-equiv *foo*))))))

(deftest test-collections
  (without-recursive-trampolines
   (testing "Empty collection literals"
     (with-strict-cps
       (verify-form-equiv [])
       (verify-form-equiv #{})
       (verify-form-equiv {})
       (verify-form-equiv ())))
   (testing "Simple collection literals"
     (with-strict-cps
       (verify-form-equiv [1 2 3])
       (verify-form-equiv #{1 2 3})
       (verify-form-equiv {:a 1 :b 2 :c 3})))
   (testing "Collection literals with complex expressions"
     (verify-form-equiv [(+ 1 2) (* 3 4) (- 10 5)])
     (verify-form-equiv #{(+ 1 2) (* 3 4) (- 10 5)})
     (verify-form-equiv {(+ 1 2) (* 3 4) :a (- 10 5) (* 10 5) :b}))
   (testing "Nested collection literals"
     (verify-form-equiv [1 2 [3 4] #{5 6} :a {7 8 9 10}])
     (verify-form-equiv #{1 2 [3 4] #{5 6} :a {7 8 9 10}})
     (verify-form-equiv {1 2 [3 4] #{5 6} :a {7 8 9 10}}))))

(deftest test-do
  (without-recursive-trampolines
   (with-strict-cps
     (verify-form-equiv (do 1 1 1)))
   (verify-form-equiv (do (+ 1 2)
                          (+ 2 3)
                          (+ 3 4)))))

(deftest test-dot
  (let [entry (new clojure.lang.MapEntry :a :b)]
    (without-recursive-trampolines
     (verify-form-equiv (. entry key))
     (verify-form-equiv (. entry (val)))
     (verify-form-equiv (+ (. (new clojure.lang.MapEntry 1 2) (key))
                           (. (new clojure.lang.MapEntry 3 4) val)))
     (verify-form-equiv (. Math sqrt (* 10 10)))
     (verify-form-equiv (. Math (sqrt (* 10 10))))
     (verify-form-equiv (Math/sqrt 250000))
     (verify-form-equiv (Math/sqrt (* 500 500)))
     (is (thrown? IllegalStateException
                  (with-strict-cps
                    (cps (. entry key))))))))

(deftest test-fn
  (without-recursive-trampolines
   (with-strict-cps
     (verify-form-equiv ((fn [] 10)))
     (verify-form-equiv ((fn [x] x) 10))
     (verify-form-equiv (let [f (fn f
                                  ([]
                                     (f 10))
                                  ([x]
                                     x))]
                          (f))))
   (verify-form-equiv ((fn [x y]
                         (+ x y))
                       1 2))
   (verify-form-equiv ((fn [& xs]
                         (reduce + xs))
                       10 11 12 13)))
  (verify-form-equiv (apply (fn [& xs]
                              (reduce * xs))
                            (range 1 10)))
  (verify-form-equiv (let [f (fn f [coll x]
                               (if (empty? coll)
                                 false
                                 (if (= x (first coll))
                                   true
                                   (f (rest coll) x))))
                           v [1 2 3]]
                       [(f v 3)])))

(deftest test-if
  (without-recursive-trampolines
   (with-strict-cps
     (verify-form-equiv (if true :a :b))
     (verify-form-equiv (if false :a :b)))
   (verify-form-equiv (if (symbol? 'x) :a :b))
   (verify-form-equiv (if (symbol? 1) :a :b))
   (verify-form-equiv (if (symbol? 'x)
                        (+ 1 2)
                        (* 3 4)))
   (verify-form-equiv (if (symbol? 1)
                        (+ 1 2)
                        (* 3 4)))))

(deftest test-let
  (without-recursive-trampolines
   (with-strict-cps
     (verify-form-equiv (let [x 1]
                          x))
     (verify-form-equiv (let [x 5]
                          (let [x 3]
                            x)))
     (verify-form-equiv (let [x 5]
                          (let [x 3])
                          x))
     (verify-form-equiv (let [x 5]
                          (let [x 3]
                            x)
                          x)))
   (verify-form-equiv (let [x 5
                            y (+ x 4)
                            x (+ x y 3)]
                        x))))

(deftest test-let-cc
  (without-recursive-trampolines
   (with-strict-cps
     (testing "normal return"
       (is (= 5
              (cps (let-cc [cc]
                     5)))))
     (testing "return via continuation"
       (is (= 5
              (cps (let-cc [cc]
                     (cc 5))))))
     (testing "call continuation from outer scope"
       ;; This is a bit of a twisted test ;-)
       ;; The continuation of the let-cc form is cc binding
       ;; so basically, the let body is executed twice.
       ;; The steps performed are:
       ;; 1) cc is bound to the continuation
       ;; 2) cc is called with id,
       ;;    effectively rebinding cc to the identity function
       ;; 3) cc is called again with id,
       ;;    resulting in the identity of the identity function
       ;; 4) the identity function is returned from the outer let
       (let [id (cps-fn [x] x)]
         (is (= id
                (cps (let [cc (let-cc [$cc]
                                $cc)]
                       (cc id)))))))
     (testing "call continuation from normal code"
       (is (= 5
              (let [cc (cps (let-cc [cc]
                              cc))]
                (cc 5))))))
   (testing "short-circuit via continuation"
     (is (= "I got here"
            (with-out-str
              (cps (let-cc [cc]
                     (print "I got here")
                     (cc nil)
                     (println "But not here")))))))))

(deftest test-letfn
  (without-recursive-trampolines
   (with-strict-cps
     (verify-form-equiv (letfn [(foo [] 10)]
                          (foo))))
   (verify-form-equiv (letfn [(even? [x]
                                (if (= x 0)
                                  true
                                  (odd? (dec x))))
                              (odd? [x]
                                (if (= x 0)
                                  false
                                  (even? (dec x))))]
                        (map even? (range 10))))))

(deftest test-new
  (without-recursive-trampolines
   (verify-form-equiv (new java.util.Hashtable))
   (verify-form-equiv (new clojure.lang.MapEntry :a :b))
   (verify-form-equiv (new clojure.lang.MapEntry (+ 1 2) (- 5 2)))))

(deftest test-quote
  (without-recursive-trampolines
   (with-strict-cps
     (verify-form-equiv 'x)
     (verify-form-equiv '(x y z))
     (verify-form-equiv '(x (y) z)))))

(deftest test-var-form
  (without-recursive-trampolines
   (with-strict-cps
     (verify-form-equiv (var *foo*))
     (verify-form-equiv (var deftest)))))

(deftest test-call-cc
  ;; Bascially the same tests as with let-cc,
  ;; except we're using call-cc
  (without-recursive-trampolines
   (with-strict-cps
     (testing "normal return"
       (is (= 5
              (cps (call-cc (fn [cc]
                              5))))))
     (testing "return via continuation"
       (is (= 5
              (cps (call-cc (fn [cc]
                              (cc 5)))))))
     (testing "call continuation from outer scope"
       ;; This is a bit of a twisted test ;-)
       ;; The continuation of the let-cc form is cc binding
       ;; so basically, the let body is executed twice.
       ;; The steps performed are:
       ;; 1) cc is bound to the continuation
       ;; 2) cc is called with id,
       ;;    effectively rebinding cc to the identity function
       ;; 3) cc is called again with id,
       ;;    resulting in the identity of the identity function
       ;; 4) the identity function is returned from the outer let
       (let [id (cps-fn [x] x)]
         (is (= id
                (cps (let [cc (call-cc (fn [$cc]
                                         $cc))]
                       (cc id)))))))
     (testing "call continuation from normal code"
       (is (= 5
              (let [cc (cps (call-cc (fn [cc]
                                       cc)))]
                (cc 5))))))
   (testing "short-circuit via continuation"
     (is (= "I got here"
            (with-out-str
              (cps (call-cc (fn [cc]
                              (print "I got here")
                              (cc nil)
                              (println "But not here"))))))))))

(deftest test-var-resolution
  (without-recursive-trampolines
   (with-strict-cps
     (verify-form-equiv (let [let-cc (fn [x]
                                       x)]
                          (let-cc 4))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Test CPS overrides of select core functions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-with-bindings
  (without-recursive-trampolines
   (with-strict-cps
     (with-bindings {#'*foo* 10}
       (verify-form-equiv *foo*))
     (verify-form-equiv (with-bindings* {#'*foo* 10}
                          (fn []
                            *foo*)))
     (verify-form-equiv (with-bindings {#'*foo* 10}
                          *foo*)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Test forbidden functions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-forbidden-fn
  (testing "push-thread-bindings"
    (is (thrown? IllegalStateException
                 ;; Use wrong number of arguments here,
                 ;; to ensure the binding stack is never changed
                 (cps (push-thread-bindings 1 2 3)))))
  (testing "pop-thread-bindings"
    (is (thrown? IllegalStateException
                 ;; Use wrong number of arguments here,
                 ;; to ensure the binding stack is never changed
                 (cps (pop-thread-bindings 1 2 3))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dynamic environment tests ($bound-fn, etc.) ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-$bound-fn
  (without-recursive-trampolines
   (testing "$bound-fn* effectively captures root binding"
     (with-strict-cps
       (is (= [0 10]
              (cps (let [f ($bound-fn [] *foo*)]
                     (binding [*foo* 10]
                       [(f) *foo*]))))))
     ;; Note:  This fails with strict-cps because the non-cps
     ;;        version fails ($bound-fn* returns a cps-fn).
     (verify-form-equiv (let [f ($bound-fn* (fn [] *foo*))]
                          (binding [*foo* 10]
                            (f)))))
   (testing "$bound-fn captures dynamic bindings"
     (with-strict-cps
       (is (= 10
              (cps ((binding [*foo* 10]
                      ($bound-fn [] *foo*)))))))
     ;; Note:  Again, this fails with strict-cps
     ;;        because the current implementation
     ;;        of $bound-fn* returns a cps-fn.
     (verify-form-equiv ((binding [*foo* 10]
                           ($bound-fn [] *foo*)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Tests for select core functions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-bound-fn
  (without-recursive-trampolines
   (with-strict-cps
     (testing "verify bound-fn can be used with strict-cps"
       (binding [*foo* 10]
         (verify-form-equiv (let [f (bound-fn []
                                      *foo*)]
                              (binding [*foo* 20]
                                (f)))))))
   (binding [*foo* 10]
     (verify-form-equiv (let [f (bound-fn [x]
                                  (+ x *foo*))]
                          (binding [*foo* 20]
                            (f *foo*)))))))
