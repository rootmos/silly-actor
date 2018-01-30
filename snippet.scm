(load "silly-actor.scm")

(pretty-print
  (and
    (atom? 'foobar)
    (not (atom? 'match))
    (verb? 'match)
    (not (verb? 'foobar))
    (not (variable-pattern? '(test Foo)))
    (variable-pattern? ',foo)
    (not (variable-pattern? ',match))
    (not (atom? '_))
    (not (atom? '_foo))
    (wildcard-pattern? '_)
    (wildcard-pattern? '_foo)
    (not (wildcard-pattern? 'foo))
    ))

(pretty-print
  (parse-Lsrc
    '(system
       [(start main)]
       (define (main s)
         [(msg1 ,x) (become empty '())]
         [(msg2 ,z) (spawn aux 2)]
         [_ (stop)])
       (define (empty s))
       (define (aux s)
         [1 (become (actor (y) [_ (stop)]) x)]
         ['() (stay x)]
         [3 (send (self) 2)]
         [_ (stay x)])
       )))
