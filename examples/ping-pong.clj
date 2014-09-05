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
