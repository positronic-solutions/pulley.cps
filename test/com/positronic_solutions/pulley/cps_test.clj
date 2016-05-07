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

(ns com.positronic-solutions.pulley.cps-test
  (:refer-clojure :exclude [trampoline])
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
     `(testing ~(with-out-str (pprint form))
        (is (= ~form
               (cps ~form)))))
  ([expected-value form]
     `(testing ~(with-out-str (pprint `(= ~expected-value
                                          ~form)))
        (is (= ~expected-value
               ~form
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

(deftest test-set!
  (without-recursive-trampolines
   (with-strict-cps
     (verify-form-equiv [[1 2] 0]
                        (let [a (binding [*foo* 1]
                                  (let [b *foo*]
                                    (set! *foo* 2)
                                    [b *foo*]))]
                          [a *foo*])))))

(deftest test-try
  (without-recursive-trampolines
   (with-strict-cps
     (verify-form-equiv (try))
     (verify-form-equiv (try 10))
     (let [;; Can't use 'new' inside with-strict-cps context
           exception (new IllegalStateException "test123")]
       (is (thrown-with-msg? IllegalStateException #"test123"
                             (cps (try (throw exception)))))
       (verify-form-equiv (try
                            (throw exception)
                            10
                            (catch IllegalStateException ex
                              ex))))
     (let [;; Can't use 'new' inside with-strict-cps context
           exception (new RuntimeException "test123")]
       (verify-form-equiv (try
                            (throw exception)
                            10
                            (catch Throwable ex
                              ex)))
       (is (thrown-with-msg? RuntimeException #"test123"
                             (cps (try
                                    (throw exception)
                                    (catch IllegalStateException ex
                                      20)))))))
   (verify-form-equiv (let [a (atom nil)]
                        (vector (try
                                  10
                                  (finally (reset! a 20)))
                                (deref a))))
   (verify-form-equiv (let [a (atom nil)
                            b (atom nil)]
                        (vector (try
                                  10
                                  (finally (reset! a 20)
                                           (reset! b 30)))
                                (deref a)
                                (deref b)))))
  (testing "Nested trampolines"
    (letfn [(foo []
              (try
                (cps (try
                       (throw (new Exception))
                       (catch Exception ex
                         (throw (new IllegalStateException)))))
                (catch IllegalStateException ex
                  (throw (new NullPointerException "Hello")))))]
      (try
        (foo)
        (catch NullPointerException ex
          (type ex)))
      (verify-form-equiv (try
                           (foo)
                           (catch NullPointerException ex
                             (type ex)))))))

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

(deftest test-with-strict-cps
  (without-recursive-trampolines
   (with-strict-cps
     (testing "Happy case"
       (verify-form-equiv 1)
       (verify-form-equiv (apply (fn [x y]
                                   [x y])
                                 '(1 2))))
     (testing "Exceptional case"
       (is (thrown? IllegalStateException
                    (cps (new NullPointerException)))))
     (testing "Catch exception within CPS context"
       (let [foo (fn [] :a)]
         (is (= :a (foo)))
         (is (= :b (cps (try
                          (foo)
                          (catch IllegalStateException ex
                            :b))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Test CPS overrides of select core functions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-instance?
  (without-recursive-trampolines
   (with-strict-cps
     (testing "Happy path"
       (verify-form-equiv
        (instance? Integer 1)))
     (testing "Non-type for class parameter"
       (is (thrown? ClassCastException
                    (instance? 1 1)))
       (is (thrown? ClassCastException
                    (cps (instance? 1 1)))))
     (testing "Catch exception within CPS context"
       (let [exception-message (fn->callable (fn [cont env ex]
                                               (. ex (getMessage))))]
         (verify-form-equiv (try
                              (instance? 1 1)
                              (catch ClassCastException ex
                                (exception-message ex)))))))))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exception Handling tests ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-call-non-callable
  (without-recursive-trampolines
   (with-strict-cps
     (let [expected-message "No implementation of method: :with-continuation of protocol: #'com.positronic-solutions.pulley.cps/ICallable found for class: java.lang.Long"
           check-exception-message (fn->callable
                                    (fn [cont env ex expected-message]
                                      (let [actual-message (. ex (getMessage))]
                                        (-> (assert (= expected-message
                                                       actual-message)
                                                    (str "Unexpected message: "
                                                         actual-message))
                                            (cont)))))]
       (testing "Unhandled Exception"
         ;; TODO: document this difference
         (is (thrown? ClassCastException
                      "java.lang.Long cannot be case to clojure.lang.IFn"
                      (1 1)))
         (is (thrown? IllegalArgumentException
                      expected-message
                      (cps (1 1)))))
       (testing "Catch exception in CPS context"
         (verify-form-equiv (try
                              (1 1)
                              ;; For non-cps version
                              (catch ClassCastException ex
                                (check-exception-message ex "java.lang.Long cannot be cast to clojure.lang.IFn"))
                              ;; For cps version
                              (catch IllegalArgumentException ex
                                (check-exception-message ex expected-message)))))))))

(deftest test-with-exception-handler
  (without-recursive-trampolines
   (with-strict-cps
     (testing "no exception"
       (verify-form-equiv :test
                          (with-exception-handler (fn [ex]
                                                    :exception)
                            :test)))
     (testing "handle exception"
       (verify-form-equiv :exception
                          (with-exception-handler (fn [ex]
                                                    :exception)
                            (1 1))))
     (testing "exception while handling exception"
       (is (thrown? NullPointerException
                    (cps (with-exception-handler (fn [ex]
                                                   (instance? nil 1))
                           (1 1))))))
     (testing "non-callable handler"
       (is (thrown-with-msg? IllegalArgumentException
                             #"No implementation of method: :with-continuation of protocol: #'com.positronic-solutions.pulley.cps/ICallable found for class: nil"
                             (cps (with-exception-handler nil
                                    (instance? nil 1))))))
     (testing "nested handlers"
       (testing "handle exception in inner handler"
         (is (= :inner-handler
                (cps (with-exception-handler (fn [ex]
                                               :outer-handler)
                       (with-exception-handler (fn [ex]
                                                 :inner-handler)
                         (1 1)))))))
       (testing "handle exception in outer handler"
         (is (= :outer-handler
                (cps (with-exception-handler (fn [ex]
                                               :outer-handler)
                       (1 (with-exception-handler (fn [ex]
                                                    :inner-handler)
                            :inner-value)))))))
       (testing "exception rethrown by inner handler"
         (is (= :outer-handler
                (cps (with-exception-handler (fn [ex]
                                               :outer-handler)
                       (with-exception-handler (fn [ex]
                                                (throw ex))
                         (1 1)))))))
       (testing "exception rethrown by both handlers"
         (is (thrown? IllegalArgumentException
                      (cps (with-exception-handler (fn [ex]
                                                     (throw ex))
                             (with-exception-handler (fn [ex]
                                                       (throw ex))
                               (1 1)))))))))
   (testing "native function as exception handler"
     (testing "handle exception"
       (let [handler (fn [ex]
                       :exception)]
         (is (= :exception
                (cps (with-exception-handler handler
                       (1 1)))))))
     (testing "exception while handling exception"
       (let [handler (fn [ex]
                       (apply nil []))]
         (is (thrown? NullPointerException
                      (cps (with-exception-handler handler
                             (1 1)))))))
     (testing "non-callable handler"
       (is (thrown-with-msg? IllegalArgumentException
                             #"No implementation of method: :with-continuation of protocol: #'com.positronic-solutions.pulley.cps/ICallable found for class: nil"
                             (cps (with-exception-handler nil
                                    (instance? nil 1))))))
     (testing "nested handlers"
       (let [outer-handler (fn [ex]
                             :outer-handler)
             inner-handler (fn [ex]
                             :inner-handler)
             rethrow-handler (fn [ex]
                               (throw ex))]
         (testing "handle exception in inner handler"
           (is (= :inner-handler
                  (cps (with-exception-handler outer-handler
                         (with-exception-handler inner-handler
                           (1 1)))))))
         (testing "handle exception in outer handler"
           (is (= :outer-handler
                  (cps (with-exception-handler outer-handler
                         (1 (with-exception-handler inner-handler
                              :inner-value)))))))
         (testing "exception rethrown by inner handler"
           (is (= :outer-handler
                  (cps (with-exception-handler outer-handler
                         (with-exception-handler rethrow-handler
                           (1 1)))))))
         (testing "exception rethrown by both handlers"
           (is (thrown? IllegalArgumentException
                        (cps (with-exception-handler rethrow-handler
                               (with-exception-handler rethrow-handler
                                 (1 1))))))))))))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Bugfixes and Regressions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_(deftest ticket-14
  ;; Proper checking for dynamic vars
  (verify-form-equiv "foo\n"
                     (with-out-str (println "foo"))))

(deftest ticket-15
  ;; References to vars def'd in the same block don't work properly
  (testing "Def without value"
    (is (= (cps (def x)
                x)
           x)))
  (testing "Def with value"
    (is (= (cps (defn f [x] x)
                (defn g [x] (f x))
                (g 1))
           1)))
  (testing "Def with value and docstring"
    (is (= (cps (def y "Docstring for y" 10)
                y))
        10))
  (testing "Make sure dynamic vars work"
    (is (= (cps (def ^:dynamic z 1)
                (binding [z 11]
                  z))
           11))))

(deftest ticket-16
  ;; Empty collection literals short-circuit continuation
  (testing "Function application with empty collections"
    (verify-form-equiv (list [] {} () #{})))
  (testing "Collection of empty collections"
    (verify-form-equiv [[] {} () #{}]))
  (testing "Destructuring with empty collection"
    (verify-form-equiv (let [[a b] [:foo {}]]
                         [a b])))
  (testing "Destructuring with 'rest' and empty collections"
    (verify-form-equiv (let [[a & b] [:foo [] {} () #{}]]
                         [a b]))))

(deftest ticket-17
  ;; (fn* [] ...) fails
  (testing "(fn* [] :foo)"
    (verify-form-equiv :foo
                       ((fn* [] :foo))))
  (testing "(fn* foo [] :foo"
    (verify-form-equiv :foo
                       ((fn* foo [] :foo))))
  (testing "recursive (fn* foo [x] ...)"
    (verify-form-equiv 0
                       ((fn* foo [x]
                          (if (> x 0)
                            (foo (dec x))
                            x))
                        10)))
  (testing "(delay :foo)"
    (verify-form-equiv :foo
                       @(delay :foo))))
