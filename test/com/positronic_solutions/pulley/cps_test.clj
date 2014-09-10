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
   #_(verify-form-equiv (+ (binding [*foo* 2
                                   *bar* (+ *foo* 5)]
                           (* *foo* *bar*))
                         (- 1 *foo* *bar*)))))

(deftest test-do
  (without-recursive-trampolines
   (with-strict-cps
     (verify-form-equiv (do 1 1 1)))
   (verify-form-equiv (do (+ 1 2)
                          (+ 2 3)
                          (+ 3 4)))))

(deftest test-fn
  (without-recursive-trampolines
   (with-strict-cps
     (verify-form-equiv ((fn [] 10)))
     (verify-form-equiv ((fn [x] x) 10)))
   (verify-form-equiv ((fn [x y]
                         (+ x y))
                       1 2))
   (verify-form-equiv ((fn [& xs]
                         (reduce + xs))
                       10 11 12 13)))
  (verify-form-equiv (apply (fn [& xs]
                              (reduce * xs))
                            (range 1 10))))

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
