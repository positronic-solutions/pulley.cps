(require '[com.positronic-solutions.cps :as cps])
(require '[criterium.core :as criterium])

(defn benchmark-factorial [n]
  (println "BENCHMARK factorial-recur")
  (criterium/bench (cps/factorial-recur n))

  (println "BENCHMARK factorial-loop")
  (criterium/bench (cps/factorial-loop n))

  (println "BENCHMARK factorial-cps1")
  (criterium/bench (cps/factorial-cps1 n))

  (println "BENCHMARK factorial-cps2")
  (criterium/bench (cps/factorial-cps2 n))

  (println "BENCHMARK factorial-cps3")
  (criterium/bench (cps/factorial-cps3 n))

  (println "BENCHMARK factorial-recur-cps")
  (criterium/bench (cps/factorial-recur-cps n)))

(defn benchmark-function [f arglists]
  (let [separator (apply str (repeat 40 "="))]
    (doseq [args arglists]
      (println separator)
      (println "args: " args)
      (println separator)

      (apply f args)
      (println))))

(defn run-benchmarks []
  (binding [cps/*allow-recursive-trampolines* false]
    (benchmark-function benchmark-factorial
                        [[10] [20] [10N] [2543N]])))

(run-benchmarks)
