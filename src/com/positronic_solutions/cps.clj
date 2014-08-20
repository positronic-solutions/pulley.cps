(ns com.positronic-solutions.cps)

(defprotocol IThunk
  (invoke-thunk [thunk]))

(defprotocol ICallable
  (with-continuation [callable cont]))

(extend-protocol ICallable
  clojure.lang.IFn
  (with-continuation [f cont]
    (fn [& args]
      (cont (apply f args)))))

(defn call [f cont & args]
  (apply (with-continuation f cont) args))

(defn trampoline
  "Runs f on a trampoline, and returns the resulting value."
  ([f & args]
     (loop [value (apply call f identity args)]
       (if (satisfies? IThunk value)
         (recur (invoke-thunk value))
         value))))

(defmacro thunk [& body]
  `(reify IThunk
     (invoke-thunk [self]
       ~@body)))

(defn fn->callable
  "Reifies f to implement both ICallable and IFn.
The result is an object that can be called with call --
e.g., within CPS-transformed code --
or directly as an IFn (e.g., in \"normal\" code).
If invoked as an IFn, it will automatically start a trampoline.

f must implement IFn, and must accept a continuation as its first argument.
Ideally, it will be CPS-transformed."
  ([f]
     (reify
       ICallable
       (with-continuation [this cont]
         (fn [& args]
           (apply f cont args)))

       clojure.lang.IFn
       (invoke [this]
         (trampoline this))
       (invoke [this arg1]
         (trampoline this arg1))
       (invoke [this arg1 arg2]
         (trampoline this arg1 arg2))
       (invoke [this arg1 arg2 arg3]
         (trampoline this arg1 arg2 arg3))
       (invoke [this arg1 arg2 arg3 arg4]
         (trampoline this arg1 arg2 arg3 arg4))
       (invoke [this arg1 arg2 arg3 arg4 arg5]
         (trampoline this arg1 arg2 arg3 arg4 arg5))
       (invoke [this arg1 arg2 arg3 arg4 arg5
                arg6]
         (trampoline this arg1 arg2 arg3 arg4 arg5
                     arg6))
       (invoke [this arg1 arg2 arg3 arg4 arg5
                arg6 arg7]
         (trampoline this arg1 arg2 arg3 arg4 arg5
                     arg6 arg7))
       (invoke [this arg1 arg2 arg3 arg4 arg5
                arg6 arg7 arg8]
         (trampoline this arg1 arg2 arg3 arg4 arg5
                     arg6 arg7 arg8))
       (invoke [this arg1 arg2 arg3 arg4 arg5
                arg6 arg7 arg8 arg9]
         (trampoline this arg1 arg2 arg3 arg4 arg5
                     arg6 arg7 arg8 arg9))
       (invoke [this arg1 arg2 arg3 arg4 arg5
                arg6 arg7 arg8 arg9 arg10]
         (trampoline this arg1 arg2 arg3 arg4 arg5
                     arg6 arg7 arg8 arg9 arg10))
       (invoke [this arg1 arg2 arg3 arg4 arg5
                arg6 arg7 arg8 arg9 arg10
                arg11]
         (trampoline this arg1 arg2 arg3 arg4 arg5
                     arg6 arg7 arg8 arg9 arg10
                     arg11))
       (invoke [this arg1 arg2 arg3 arg4 arg5
                arg6 arg7 arg8 arg9 arg10
                arg11 arg12]
         (trampoline this arg1 arg2 arg3 arg4 arg5
                     arg6 arg7 arg8 arg9 arg10
                     arg11 arg12))
       (invoke [this arg1 arg2 arg3 arg4 arg5
                arg6 arg7 arg8 arg9 arg10
                arg11 arg12 arg13]
         (trampoline this arg1 arg2 arg3 arg4 arg5
                     arg6 arg7 arg8 arg9 arg10
                     arg11 arg12 arg13))
       (invoke [this arg1 arg2 arg3 arg4 arg5
                arg6 arg7 arg8 arg9 arg10
                arg11 arg12 arg13 arg14]
         (trampoline this arg1 arg2 arg3 arg4 arg5
                     arg6 arg7 arg8 arg9 arg10
                     arg11 arg12 arg13 arg14))
       (invoke [this arg1 arg2 arg3 arg4 arg5
                arg6 arg7 arg8 arg9 arg10
                arg11 arg12 arg13 arg14 arg15]
         (trampoline this arg1 arg2 arg3 arg4 arg5
                     arg6 arg7 arg8 arg9 arg10
                     arg11 arg12 arg13 arg14 arg15))
       (invoke [this arg1 arg2 arg3 arg4 arg5
                arg6 arg7 arg8 arg9 arg10
                arg11 arg12 arg13 arg14 arg15
                arg16]
         (trampoline this arg1 arg2 arg3 arg4 arg5
                     arg6 arg7 arg8 arg9 arg10
                     arg11 arg12 arg13 arg14 arg15
                     arg16))
       (invoke [this arg1 arg2 arg3 arg4 arg5
                arg6 arg7 arg8 arg9 arg10
                arg11 arg12 arg13 arg14 arg15
                arg16 arg17]
         (trampoline this arg1 arg2 arg3 arg4 arg5
                     arg6 arg7 arg8 arg9 arg10
                     arg11 arg12 arg13 arg14 arg15
                     arg16 arg17))
       (invoke [this arg1 arg2 arg3 arg4 arg5
                arg6 arg7 arg8 arg9 arg10
                arg11 arg12 arg13 arg14 arg15
                arg16 arg17 arg18]
         (trampoline this arg1 arg2 arg3 arg4 arg5
                     arg6 arg7 arg8 arg9 arg10
                     arg11 arg12 arg13 arg14 arg15
                     arg16 arg17 arg18))
       (invoke [this arg1 arg2 arg3 arg4 arg5
                arg6 arg7 arg8 arg9 arg10
                arg11 arg12 arg13 arg14 arg15
                arg16 arg17 arg18 arg19]
         (trampoline this arg1 arg2 arg3 arg4 arg5
                     arg6 arg7 arg8 arg9 arg10
                     arg11 arg12 arg13 arg14 arg15
                     arg16 arg17 arg18 arg19))
       (invoke [this arg1 arg2 arg3 arg4 arg5
                arg6 arg7 arg8 arg9 arg10
                arg11 arg12 arg13 arg14 arg15
                arg16 arg17 arg18 arg19 arg20]
         (trampoline this arg1 arg2 arg3 arg4 arg5
                     arg6 arg7 arg8 arg9 arg10
                     arg11 arg12 arg13 arg14 arg15
                     arg16 arg17 arg18 arg19 arg20))
       (invoke [this arg1 arg2 arg3 arg4 arg5
                arg6 arg7 arg8 arg9 arg10
                arg11 arg12 arg13 arg14 arg15
                arg16 arg17 arg18 arg19 arg20 more]
         (apply (trampoline this arg1 arg2 arg3 arg4 arg5
                            arg6 arg7 arg8 arg9 arg10
                            arg11 arg12 arg13 arg14 arg15
                            arg16 arg17 arg18 arg19 arg20 more)))
       (applyTo [this args]
         (clojure.lang.AFn/applyToHelper this args)))))

;; This example illustrates minimal transformation,
;; with manual priming of the trampoline
(defn factorial-cps1 [n]
  (letfn [(factorial-cont [cont n acc]
            (if (> n 0)
              ;; (factorial-cont (dec n) (* acc n))
              (thunk (factorial-cont cont (dec n) (* acc n)))
              ;; return acc
              (thunk (cont acc))))]
    (trampoline factorial-cont identity n 1)))

;; This example illustrates a moderate degree of transformation,
;; including how letfn could be transformed,
;; and provides for transparent priming of the trampoline
(def factorial-cps2
  (reify
    ICallable
    (with-continuation [self cont]
      (fn [n]
        ;; letfn shenanigans
        ;; Inspired in part by
        ;; http://cs.brown.edu/courses/cs173/2012/book/Control_Operations.html#(part._.Continuation-.Passing_.Style)
        ((fn [factorial-tco]
           (deliver factorial-tco
                    (reify
                      ICallable
                      (with-continuation [self cont]
                        (fn [n acc]
                          (if (> n 0)
                            (thunk (call @factorial-tco cont (dec n) (* acc n)))
                            (thunk (cont acc)))))

                      clojure.lang.IFn
                      (invoke [self n acc]
                        (trampoline self n acc))))
           (thunk (call @factorial-tco cont n 1)))
         (promise))))

    clojure.lang.IFn
    (invoke [self n]
      (trampoline self n))))

;; Basically the same as factorial-cps2,
;; but simplified using fn->callable
(def factorial-cps3
  (fn->callable (fn [cont n]
                  ;; letfn shenanigans
                  ;; Inspired in part by
                  ;; http://cs.brown.edu/courses/cs173/2012/book/Control_Operations.html#(part._.Continuation-.Passing_.Style)
                  ((fn [factorial-tco]
                     (deliver factorial-tco
                              (reify
                                ICallable
                                (with-continuation [self cont]
                                  (fn [n acc]
                                    (if (> n 0)
                                      (thunk (call @factorial-tco cont (dec n) (* acc n)))
                                      (thunk (cont acc)))))

                                clojure.lang.IFn
                                (invoke [self n acc]
                                  (trampoline self n acc))))
                     (thunk (call @factorial-tco cont n 1)))
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

#_(def-cps factorial-cps [n]
  (letfn [(factorial-tco [n acc]
            (if (> n 0)
              (factorial-tco (dec n) (* n acc))
              acc))]
    (factorial-tco n 1)))
