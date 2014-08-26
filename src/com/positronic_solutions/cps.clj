(ns com.positronic-solutions.cps)

(def ^:dynamic *trampoline-depth*
  "Records the number of trampolines active on the current stack."
  0)

(def ^:dynamic *allow-recursive-trampolines*
  "Are we allowed to start another trampoline if there's already one on the stack?

Default: true"
  true)

(def ^:dynamic *strict-cps*
  "If true, we are not allowed to make calls to CPS functions.
Otherwise, we can mix functions any way we like.

Default: false"
  false)

(defmacro with-strict-cps [& body]
  `(binding [*strict-cps* true]
     ~@body))

(defmacro without-recursive-trampolines [& body]
  `(binding [*allow-recursive-trampolines* false]
     ~@body))

(defprotocol IThunk
  (invoke-thunk [thunk]))

(defprotocol ICallable
  (with-continuation [callable cont]))

(extend-protocol ICallable
  clojure.lang.IFn
  (with-continuation [f cont]
    (fn [& args]
      (when *strict-cps*
        (throw (new IllegalStateException "Attempt to call non-CPS routine while *strict-cps* is set.")))
      ;; Should cont be applied in a thunk?
      (cont (apply f args)))))

(defn call [f cont & args]
  (apply (with-continuation f cont) args))

(defn trampoline
  "Runs f on a trampoline, and returns the resulting value."
  ([f & args]
     (if (or *allow-recursive-trampolines*
             (= *trampoline-depth* 0))
       (binding [*trampoline-depth* (inc *trampoline-depth*)]
         (loop [value (apply call f identity args)]
           (if (satisfies? IThunk value)
             (recur (invoke-thunk value))
             value)))
       (throw (new IllegalStateException "Attempt to invoke recursive trampoline, but *allow-recursive-trampolines* does not allow it.")))))

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

(defmacro cps [& body]
  ;; Create a cps-fn, and apply it
  `((cps-fn* nil
             ([]
                ~@body))))

(defmacro cps-expr [cont expr]
  #_(println "expr: " expr)
  (cond (seq? expr) `(cps-form ~cont ~expr)
        (coll? expr) `(cps-coll ~cont ~expr)
        ;; Otherwise, should be a literal or simple expression (symbol)
        ;; Maybe this should be thunk'd in some (or all) cases
        :else `(~cont ~expr)))

(defmacro cps-coll [cont coll]
  (throw (new IllegalStateException "Collection literals are not cupported at this time.")))

(defmacro cps-form [cont form]
  #_(println "form: " form)
  (let [expanded (macroexpand form)
        [action & body] expanded
        f (gensym)]
    (if (special-symbol? action)
      `(cps-special-form ~cont ~expanded)
      `(cps-call ~cont ~@expanded))))

(defmacro cps-call
  "Macro that transforms a function call.

Parameters:
  cont - this form's continuation form
  f - the form of the function to be called
  args - the forms of the function arguments"
  ([cont f & args]
     (let [value (gensym "value_")]
       `(cps-expr (fn [~value]
                    (cps-apply ~cont ~value [] ~args))
                  ~f))))

(defmacro cps-apply
  " Helper macro used by cps-call for transforming function calls.

Parameters:
  cont - this form's continuation form
  f - a symbol representing the function to be called
  evaled - a vector of symbols representing values
           for function argument expressions
           that have been transformed so far
  unevaled - a vector containing forms for function arguments
             that still need to be transformed"
  ([cont f evaled unevaled]
     (if (empty? unevaled)
       ;; then (we have values for all the arguments -> call function)
       `(thunk (call ~f ~cont ~@evaled))
       ;; else (we need a value for at least one more argument)
       (let [value (gensym "value_")]
         `(cps-expr (fn [~value]
                      (cps-apply ~cont ~f
                                 ~(conj evaled value)
                                 ~(rest unevaled)))
                    ;; evaluate first unevaluated argument
                    ~(first unevaled))))))

(defmacro cps-special-form [cont [action & body]]
  (case action
    def `(cps-def ~cont ~@body)
    do `(cps-do ~cont ~@body)
    fn* `(cps-fn* ~cont ~@body)
    if `(cps-if ~cont ~@body)
    let* `(cps-let* ~cont ~@body)))

(defmacro cps-def
  ([cont name]
     `(~cont (def ~name)))
  ([cont name expr]
     (let [value (gensym "value_")]
       `(cps-expr (fn [~value]
                    (~cont (def ~name ~value)))
                  ~expr)))
  ([cont name doc expr]
     (let [value (gensym "value_")]
       `(cps-expr (fn [~value]
                    (~cont (def ~name ~doc ~value)))
                  ~expr))))

(defmacro cps-fn*
  "Constructs a CPS-transformed function

If cont is not nil, it will be called with the resulting function.
Otherwise, the resulting form will evaluate direcly to the function."
  ([cont & bodies]
     (let [return (gensym)]
       ;; f = constructed function
       (let [f `(fn->callable (fn ~@(for [spec bodies]
                                      (if (symbol? spec)
                                        spec
                                        (let [[params & body] spec]
                                          #_(println "fn: " params body)
                                          `(~(into []
                                                   (cons return params))
                                            (cps-do ~return ~@body)))))))]
         (if cont
           ;; then (continuation provided -> pass f to continuation)
           `(~cont ~f)
           ;; else (continuation not provided -> return f directory)
           f)))))

(defmacro cps-if
  ([cont test then]
     `(cps-if ~cont ~test ~then nil))
  ([cont test then else]
     (let [v (gensym)
           cont-fn (gensym "continuation_")]
       `(cps-expr (fn [~v]
                    (let [~cont-fn ~cont]
                      (if ~v
                        (cps-expr ~cont-fn ~then)
                        (cps-expr ~cont-fn ~else))))
                  ~test))))

(defmacro cps-let* [cont bindings & body]
  (if (empty? bindings)
    ;; then (no more bindings => evaluate body)
    `(cps-do ~cont
             ~@body)
    ;; else (at least one more binding => evaluate and bind first binding)
    (let [[name expr & rest-bindings] bindings]
      `(cps-expr (fn [~name]
                   (cps-let* ~cont ~rest-bindings
                             ~@body))
                 ~expr))))

(defmacro cps-do
  ([cont]
     `(~cont nil))
  ([cont expr & body]
     (let [value (gensym "value_")]
       (if (empty? body)
         `(cps-expr (fn [~value]
                      (~cont ~value))
                    ~expr)
         `(cps-expr (fn [~value]
                      (cps-do ~cont
                              ~@body))
                    ~expr)))))

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

#_(cps (defn factorial-cps [n]
       (letfn [(factorial-tco [n acc]
                 (if (> n 0)
                   (factorial-tco (dec n) (* n acc))
                   acc))]
         (factorial-tco n 1))))

(cps (defn factorial-recur-cps [n]
       (if (> n 0)
         (* n (factorial-recur-cps (dec n)))
         1)))
