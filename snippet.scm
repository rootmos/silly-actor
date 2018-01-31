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
    (not (variable-pattern? ',match))
    (not (atom? '_))
    (not (atom? '_foo))
    (wildcard-pattern? '_)
    (wildcard-pattern? '_foo)
    (not (wildcard-pattern? 'foo))
    ))

(define sample
  '(system
     [(init main) (init empty)]
     (define (main s)
       [(msg1 ,x) (become empty '())]
       [(msg2 ,z) (spawn aux 2)]
       [_ (stop)])
     (define (empty _))
     (define (aux s)
       [1 (become (actor (y) [_ (stop)]) x)]
       ['() (stay x)]
       [3 (send (self) 2)]
       [_ (stay x)])))

(define sample-system (output-scheme (parse-Lsrc sample)))
(run-system sample-system)
