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

(ns com.positronic-solutions.pulley.cps
  (:require [clojure.repl :as repl]))

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
  (with-continuation [callable cont env]))

(extend-protocol ICallable
  clojure.lang.IFn
  (with-continuation [f cont env]
    (fn [& args]
      #_(println "Calling native fn: " f args)
      (when *strict-cps*
        (throw (new IllegalStateException
                    (str "Attempt to call non-CPS routine "
                         f
                         " while *strict-cps* is set."))))
      (let [value (with-bindings env
                    (apply f args))]
        (cont value)))))

(defn call [f cont env & args]
  #_(println "call: continuation is " cont)
  #_(println "call: env is " env)
  #_(println "calling " f args)
  ;; TODO: thunk this (so we don't have to thunk it everywhere it's called)?
  (apply (with-continuation f cont env) args))

(defn trampoline
  "Runs f on a trampoline, and returns the resulting value."
  ([f & args]
     (if (or *allow-recursive-trampolines*
             (= *trampoline-depth* 0))
       (binding [*trampoline-depth* (inc *trampoline-depth*)]
         ;; TODO: should we pass the current thread bindings
         ;;       as the initial dynamic enviroment,
         ;;       rather than an empty map?
         ;;       (unfortunately, that is pretty detrimental to performance)
         (loop [value (apply call f identity {} args)]
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
       (with-continuation [this cont env]
         (fn [& args]
           (apply f cont env args)))

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

(defmacro let-cc
  "Executes body with <cc> bound to the current continuation.

Usage:

    (let-cc [<cc>]
      <body...>"
  ([[cc] & body]
    (throw (new IllegalStateException "let-cc can only be used inside cps and cps-fn forms"))))

(def ^:dynamic *special-form-handlers*
  "Contains a map specifying how special forms are handled.

We need to treat certain forms as \"special\".
Often, these are Clojure \"special forms\",
but there are other forms we need to treat specially as well —
such as 'binding' forms (which Clojure implements as a macro).

This var provides a unified way of detecting these cases
and dispatching to an appropriate handler function.
It is also dynamic, allowing compilers built on top of the cps compiler
to implement their own special forms.

Clojure special forms are handled by mapping its \"special symbol\"
to a handler function.
Other forms are handled by mapping the var object
representing the operator of the form (i.e., #'binding)
to a handler function.
The handler function must accept the following parameters (in order)
* form - the form to be expanded
* macro-env - the macro environment
* continuation - a form representing the current continuation
                 of the form being expanded
* dynamic-env - a symbol representing the dynamic environment
                that will be available when the form is executed
                (i.e., a map containing the var->value bindings
                for dynamic vars)"
  {'. (fn expand-dot [[operator & body] &env cont env]
        `(cps-dot ~cont ~env ~@body))
   #'binding (fn expand-binding [[operator & body] &env cont env]
               `(cps-binding ~cont ~env ~@body))
   'def (fn expand-def [[operator & body] &env cont env]
          `(cps-def ~cont ~env ~@body))
   'do (fn expand-do [[operator & body] &env cont env]
         `(cps-do ~cont ~env ~@body))
   'fn* (fn expand-fn* [[operator & body] &env cont env]
          `(cps-fn* ~cont ~env ~@body))
   'if (fn expand-if [[operator & body] &env cont env]
         `(cps-if ~cont ~env ~@body))
   'let* (fn expand-let* [[operator & body] &env cont env]
           `(cps-let* ~cont ~env ~@body))
   #'let-cc (fn expand-let-cc [[operator & body] &env cont env]
              `(cps-let-cc ~cont ~env ~@body))
   'letfn* (fn expand-letfn* [[operator & body] &env cont env]
             `(cps-letfn* ~cont ~env ~@body))
   'new (fn expand-new [[operator & body] &env cont env]
          `(cps-new ~cont ~env ~@body))
   'quote (fn expand-quote [[operator & body] &env cont env]
            `(cps-quote ~cont ~env ~@body))
   'set! (fn expand-set! [[operator & body] &env cont env]
           `(cps-set! ~cont ~env ~@body))
   'var (fn expand-var [[operator & body] &env cont env]
          `(cps-var ~cont ~env ~@body))})

(defmacro cps [& body]
  ;; Create a cps-fn, and apply it
  `((cps-fn* nil
             nil
             ([]
                ~@body))))

(defmacro cps-fn
  ;; This is a public macro for generating CPS-transformed functions
  "Generates a CPS-transformed function from the given body(ies).
Simply specify the function the same way you would use fn."
  ([& body]
     `(cps-expr nil
                nil
                (fn ~@body))))

(defmacro cps-expr [cont env expr]
  #_(println "expr: " expr)
  (cond (seq? expr) `(cps-form ~cont ~env ~expr)
        (coll? expr) `(cps-coll ~cont ~env ~expr)
        ;; We need to handle dynamic vars specially
        (symbol? expr) `(cps-symbol ~cont ~env ~expr)
        ;; Otherwise, should be a literal expression
        :else `(~cont ~expr)))

(defmacro cps-exprs
  "Low-level macro for transforming a sequence of expressions
and binding their values to variables.

After all expressions have been processed,
callback is called with a collection of symbols
which will be bound to the values of the expressions.
callback should return a form,
which will be executed in a scope with all these variables.
Note that callback must be an actual function,
callable at macro-expansion time,
not a form representing a function."
  ([env exprs callback]
     (if (empty? exprs)
       ;; then (no exprs => invoke callback with empty collection
       (callback nil)
       ;; else (process first expression, then rest recursively)
       (let [value (gensym "value_")
             new-callback (fn [values]
                            (callback (cons value values)))]
         `(cps-expr (fn [~value]
                      (cps-exprs ~env ~(rest exprs) ~new-callback))
                    ~env
                    ~(first exprs))))))

(defmacro cps-coll
  ([cont env coll]
     (cond
       ;; MapEntry's need to be handled specially,
       ;; because empty returns nil for them.
       ;; At least for now, we simply convert them to vectors.
       (instance? clojure.lang.MapEntry coll)
       `(cps-coll ~cont ~env ~(vec coll))

       ;; Handle every other case
       :else
       `(cps-exprs ~env
                   ~(seq coll)
                   ~(fn [vars]
                      (if-let [empty-coll (empty coll)]
                        `(~cont (conj ~(empty coll)
                                      ~@vars))
                        (throw (new IllegalStateException
                                    (str "Conversion of literal "
                                         (print-str (type coll))
                                         " is not supported at this time")))))))))

(defn dynamic? [resolved-var]
  (:dynamic (meta resolved-var)))

(defmacro cps-symbol [cont env name]
  (if-let [resolved (resolve &env name)]
    ;; then (symbol resolves to a var => see if it's dynamic)
    (if (dynamic? resolved)
      ;; then (dynamic => do dynamic lookup against env)
      (let [k (gensym "key_")
            v (gensym "value_")]
        `(~cont (if-let [[~k ~v] (find ~env ~resolved)]
                  ;; then (binding was found in env => use its value)
                  ~v
                  ;; else (use the enclosing environment's value)
                  ~name)))
      ;; else (not dynamic => just evaluate the symbol directly)
      `(~cont ~name))
    ;; else (local / unresolved => just evaluate the symbol directly)
    `(~cont ~name)))

(defmacro cps-form [cont env form]
  #_(println "form: " form)
  (let [[operator & operands] form]
    ;; Check to see if we need to handle the form specially
    (if (and (symbol? operator)
             (or (special-symbol? operator)
                 (contains? *special-form-handlers* (resolve &env operator))))
      ;; then (handle as a "special form")
      `(cps-special-form ~cont ~env ~form)
      ;; else (attempt to expand)
      (let [expanded (macroexpand-1 form)]
        (if (identical? form expanded)
          ;; then (expansion complete => handle as function call)
          `(cps-call ~cont ~env ~@expanded)
          ;; else (transform expanded expression)
          ;; Note: the expansion could be an atomic expression,
          ;;       so we use cps-expr here.
          `(cps-expr ~cont ~env ~expanded))))))

(defmacro cps-call
  "Macro that transforms a function call.

Parameters:
  cont - this form's continuation form
  f - the form of the function to be called
  args - the forms of the function arguments"
  ([cont env f & args]
     (let [value (gensym "value_")]
       `(cps-expr (fn [~value]
                    (cps-apply ~cont ~env ~value [] ~args))
                  ~env
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
  ([cont env f evaled unevaled]
     (if (empty? unevaled)
       ;; then (we have values for all the arguments -> call function)
       `(thunk (call ~f ~cont ~env ~@evaled))
       ;; else (we need a value for at least one more argument)
       (let [value (gensym "value_")]
         `(cps-expr (fn [~value]
                      (cps-apply ~cont ~env ~f
                                 ~(conj evaled value)
                                 ~(rest unevaled)))
                    ~env
                    ;; evaluate first unevaluated argument
                    ~(first unevaled))))))

(defmacro cps-special-form [cont env form]
  (let [[operator & body] form
        operator-id (if (special-symbol? operator)
                        operator
                        (resolve operator))
        handler (get *special-form-handlers* operator-id)]
    (if handler
      ;; then (apply handler)
      (handler form &env cont env)
      ;; else (throw exception)
      (throw (new UnsupportedOperationException
                  (str "No CPS transformer found for form: " operator))))))

(defmacro cps-binding
  ([cont env bindings & body]
     (when (not (vector? bindings))
       (throw (new IllegalArgumentException
                   (str "binding requires a vector for its binding"))))
     (if (empty? bindings)
       ;; then (simply execute the body)
       `(cps-do ~cont ~env
                ~@body)
       ;; else (process bindings)
       `(cps-exprs ~env
          ;; binding expressions
          ~(for [[name expr] (partition-all 2 bindings)]
             expr)
          ;; callback function
          ~(fn [values]
             (let [new-env (gensym "env_")]
               `(let [~new-env
                      (-> ~env
                          ~@(map (fn [[name _] value]
                                   (let [binding-var (resolve name)]
                                     (when (not (dynamic? binding-var))
                                       (throw (new IllegalStateException
                                                   (str "Can't dynamically bind non-dynamic var: "
                                                        binding-var))))
                                     `(assoc ~binding-var ~value)))
                                 (partition-all 2 bindings)
                                 values))]
                  (cps-do ~cont ~new-env
                          ~@body))))))))

(defmacro cps-def
  ([cont env name]
     `(~cont (def ~name)))
  ([cont env name expr]
     (let [value (gensym "value_")]
       `(cps-expr (fn [~value]
                    (~cont (def ~name ~value)))
                  ~env
                  ~expr)))
  ([cont env name doc expr]
     (let [value (gensym "value_")]
       `(cps-expr (fn [~value]
                    (~cont (def ~name ~doc ~value)))
                  ~env
                  ~expr))))

(defmacro cps-dot
  ([cont env & body]
     (when (< (count body) 2)
       (throw (new IllegalArgumentException
                   (str "Malformed member expression: "
                        (cons '. body)))))
     (let [[obj member & args] body]
       (if (and (empty? args)
                (seq? member))
         ;; then (expand sequence of arguments)
         (if (symbol? (first member))
           `(cps-dot ~cont ~env ~obj ~@member)
           (throw (new IllegalArgumentException
                       (str "Malformed member expression: "
                            (cons '. body)))))
         ;; else (process member expression)
         (let [v (gensym "v_")]
           (let [obj-is-class (and (symbol? obj)
                                   (class? (resolve &env obj)))
                 k (fn [params]
                     `(thunk (call (fn []
                                     (. ~(if obj-is-class
                                           ;; then (use class directly)
                                           obj
                                           ;; else (use bound value)
                                           v)
                                        ~member
                                        ~@params))
                                   ~cont
                                   ~env)))]
             (if obj-is-class
               ;; then (use class directly)
               `(cps-exprs ~env ~args ~k)
               ;; else (bind value)
               `(cps-expr (fn [~v]
                            (cps-exprs ~env ~args ~k))
                          ~env
                          ~obj))))))))

(defmacro cps-fn*
  "Constructs a CPS-transformed function

If cont is not nil, it will be called with the resulting function.
Otherwise, the resulting form will evaluate direcly to the function."
  ([cont env & bodies]
     (let [return (gensym)
           env (gensym "env_")]
       ;; f = constructed function
       (let [f `(fn->callable (fn ~@(for [spec bodies
                                          ;; Keeping the symbol
                                          ;; for a function (if any)
                                          ;; is interfering with
                                          ;; the current implementation
                                          ;; of letfn* (it introduces
                                          ;; a binding for the symbol?),
                                          ;; so just filter out symbols
                                          ;; for now.
                                          :when (not (symbol? spec))]
                                      ;; This should never be true,
                                      ;; since we filter out symbols
                                      ;; in the :when clause,
                                      ;; but I'm keeping it around
                                      ;; because ideally, we want
                                      ;; to keep meaningful function names.
                                      ;; Maybe we should convert symbols
                                      ;; to (gensym (name spec))?
                                      (if (symbol? spec)
                                        spec
                                        (let [[params & body] spec]
                                          #_(println "fn: " params body)
                                          `(~(into []
                                                   (concat [return env]
                                                           params))
                                            (cps-do ~return ~env
                                                    ~@body)))))))]
         (if cont
           ;; then (continuation provided -> pass f to continuation)
           `(~cont ~f)
           ;; else (continuation not provided -> return f directory)
           f)))))

(defmacro cps-if
  ([cont env test then]
     `(cps-if ~cont ~env ~test ~then nil))
  ([cont env test then else]
     (let [v (gensym)
           cont-fn (gensym "continuation_")]
       `(cps-expr (fn [~v]
                    (let [~cont-fn ~cont]
                      (if ~v
                        (cps-expr ~cont-fn ~env ~then)
                        (cps-expr ~cont-fn ~env ~else))))
                  ~env
                  ~test))))

(defmacro cps-let* [cont env bindings & body]
  (if (empty? bindings)
    ;; then (no more bindings => evaluate body)
    `(cps-do ~cont ~env
             ~@body)
    ;; else (at least one more binding => evaluate and bind first binding)
    (let [contv (gensym "continuation_")
          [name expr & rest-bindings] bindings]
      `(let [~contv ~cont]
         (cps-expr (fn [~name]
                     (cps-let* ~contv ~env
                               ~rest-bindings
                               ~@body))
                   ~env
                   ~expr)))))

(defmacro cps-let-cc
  "CPS-aware macro for expanding a let-cc form"
  ([cont env [cc] & body]
     (let [contv (gensym "continuation_")]
       `(let [~contv ~cont
              ~cc (fn->callable (fn [~'$cont ~'$env ~'$value]
                                  (~contv ~'$value)))]
          (cps-do ~contv ~env
                  ~@body)))))

(defmacro cps-letfn*
  ([cont env bindings & body]
     ;; I would much prefer to expand this to a letfn* form instead,
     ;; with appropriate CPS-routine definitions instead of fn forms.
     ;; However, letfn* appears to choke unless the value expressions
     ;; are fn forms.
     (let [bindings-info (for [binding (partition-all 2 bindings)]
                           {:name (first binding)
                            :promise-name (gensym "promise_")
                            :fn-form (second binding)})
           contv (gensym "cont_")
           fn-env (gensym "env_")
           args (gensym "args_")]
       `(let [~@(->> (for [binding bindings-info]
                       [(:promise-name binding) `(promise)
                        (:name binding) `(-> (fn [~contv ~fn-env & ~args]
                                               (apply call
                                                      (deref ~(:promise-name binding))
                                                      ~contv
                                                      ~fn-env
                                                      ~args))
                                             (fn->callable))])
                     (apply concat))]
          ~@(for [binding bindings-info]
              `(deliver ~(:promise-name binding)
                        (cps-form nil ~env
                                  ~(:fn-form binding))))
          (cps-do ~cont ~env
                  ~@body)))))

(defmacro cps-new
  ([cont env class & params]
     (let [f (gensym "new_")]
       `(cps-exprs ~env
                   ~params
                   ~(fn [params]
                      `(let [~f (fn []
                                  (new ~class ~@params))]
                         (thunk (call ~f ~cont ~env))))))))

(defmacro cps-quote
  ([cont env & body]
     `(~cont (quote ~@body))))

(defmacro cps-set!
  ([cont env place expr]
     ;; TODO: I don't think there's a case where set!
     ;; is allowed on a var except when it's dynamic,
     ;; and we don't really support mutable dynamic environments yet,
     ;; so just disallow any attempt to set! a symbol.
     (when (symbol? place)
       (throw (new UnsupportedOperationException
                   "Using set! on vars is not supported yet in CPS code")))
     (let [value (gensym "value_")]
       `(cps-expr (fn [~value]
                    (~cont (set! ~place ~value)))
                  ~env
                  ~expr))))

(defmacro cps-var
  ([cont env symbol]
     `(~cont (var ~symbol))))

(defmacro cps-do
  ([cont env]
     `(~cont nil))
  ([cont env expr & body]
     (let [value (gensym "value_")]
       (if (empty? body)
         `(cps-expr ~cont ~env
                    ~expr)
         `(cps-expr (fn [~value]
                      (cps-do ~cont ~env
                              ~@body))
                    ~env
                    ~expr)))))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Runtime Library ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def call-cc
  (cps-fn [f]
    (let-cc [cc]
      (f cc))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Helper macros for generating CPS overrides of native functions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro override-fn
  "Used to provide a CPS-transformed version of an existing native function.

This macro generates a CPS override for the specified function
from the provided implementation code (in fn-tails).
The provided implementation code is CPS-transformed
and will be called (as a CPS routine) when the specified function is called
from within another CPS routine.
This effectively replaces the native implementation
with a CPS implementation in such cases.

name is the function to override.

fn-tails are function parameter list(s) and body(ies),
as would be used in a fn or cps-fn form.
These are enclosed in a cps-fn form and sent through the CPS compiler."
  ([name & fn-tails]
     (let [f (gensym "f_")]
       `(let [~f (cps-fn ~@fn-tails)]
          (extend-type (type ~name)
            ICallable
            (with-continuation [~'self ~'cont ~'env]
              (with-continuation ~f ~'cont ~'env)))))))

(defmacro auto-override-fn
  "Attempts to automatically generate a CPS override for a function
from its native definition.
This requires that the source code be for the function
is discoverable.
Currently, this is done via clojure.repl/source-fn.
If the source for the function can not be located,
a compile-time exception is thrown.

name is the name (symbol) of the function to override.

This macro is useful to cases where
a) the source code for a function is discoverable, and
b) that code can be transformed as-is by the CPS compiler
   (i.e., it does not contain unsupported forms)
If those conditions are satisfied, this macro is a great way
to provide a CPS implementation of a function
without the need to duplicate the code."
  ([name]
     ;; TODO: Is there a better way to get the source
     ;;       for a function than relying on clojure.repl?
     (if-let [code (repl/source-fn name)]
       ;; then (got source)
       (let [fn-tails (-> code
                          (read-string)
                          (macroexpand)
                          (nth 2)
                          (rest))]
         `(override-fn ~name ~@fn-tails))
       ;; else (couldn't get source)
       (throw (new IllegalStateException
                   (str "Couldn't find source for function " name))))))

(defn forbid-fn!
  "Provides a \"CPS Override\" for a fn that throws an IllegalStateException,
thus effectively preventing the function from being called from a CPS context."
  ([f]
     (forbid-fn! f (str "Calling " f " from CPS code is not allowed.")))
  ([f msg]
     (extend-type (type f)
        ICallable
        (with-continuation [self cont env]
          (throw (new IllegalStateException msg))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CPS overrides of select core functions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(auto-override-fn bound-fn*)

;; CPS override of apply
(extend-type (type apply)
  ICallable
  (with-continuation [self cont env]
    (fn [f & args]
      (apply call f cont env (apply list* args)))))

;; CPS override of get-thread-bindings
(extend-type (type get-thread-bindings)
  ICallable
  (with-continuation [self cont env]
    (fn []
      (cont (merge (get-thread-bindings)
                   env)))))

;; CPS override of with-bindings*
(extend-type (type with-bindings*)
  ICallable
  (with-continuation [self cont env]
    (fn [binding-map f & args]
      (let [full-env (merge-with (fn [a b]
                                   b)
                                 env
                                 binding-map)]
        (thunk (apply call f cont full-env args))))))

;; Forbid push-thread-bindings
(forbid-fn! push-thread-bindings
            "push-thread-bindings/pop-thread-bindings can not be used in CPS code.  Use a higher-level construct, such as with-bindings, instead.")

;; Forbid pop-thread-bindings
(forbid-fn! pop-thread-bindings
            "push-thread-bindings/pop-thread-bindings can not be used in CPS code.  Use a higher-level construct, such as with-bindings, instead.")
