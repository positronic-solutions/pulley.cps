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

(call identity identity 1)

(defn trampoline [f & args]
  (loop [value (apply call f identity args)]
    (if (satisfies? IThunk value)
      (recur (invoke-thunk value))
      value)))

(defmacro thunk [& body]
  `(reify IThunk
     (invoke-thunk [self]
       ~@body)))

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
