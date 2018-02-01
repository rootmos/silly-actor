(load "silly-actor.scm")

(assert
  (and
    (atom? 'foobar)
    (not (atom? 'match))
    (not (atom? 'System_msg))
    (verb? 'match)
    (not (verb? 'foobar))
    (not (variable-pattern? '(test foo)))
    (variable-pattern? ',foo)
    (variable-pattern? ',x)
    (not (variable-pattern? ',match))
    (not (atom? '_))
    (not (atom? '_foo))
    (wildcard-pattern? '_)
    (wildcard-pattern? '_foo)
    (not (wildcard-pattern? 'foo))
    ))

(define sample
  '(system
     [(init main 0) (init empty '())]
     (define (main)
       [(var x) (become (var empty) (value '()))]
       [((atom msg2) (var z)) (spawn (var aux) (value 2))]
       [_ (stop)])
     (define (empty))
     (define (aux)
       [1 (become (value (actor ((var x) (stop)))) (var x))]
       ['() (stay (var x))]
       [3 (send (self) (value 2))]
       [_ (stay (var x))])
     ))

(define sample-system (output-scheme (parse-Lsrc sample)))
(pretty-print sample-system)
(eval sample-system (environment '(scheme) '(runtime)))
