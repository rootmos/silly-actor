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
       [_ (output (value (atom lol)))]
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

(define (interp x)
  (let ([code (output-scheme (parse-Lsrc x))])
    (eval code (environment '(scheme) '(runtime)))))

(define (test code expected)
  (let ([str (call-with-string-output-port (lambda (p) (interp (code p))))])
    (with-input-from-string str (lambda ()
      (let ([actual (reverse (let go ([acc '()])
                               (let ([o (read)])
                                 (cond
                                   [(eqv? o (eof-object)) acc]
                                   [else (go (cons o acc))]))))])
        (printf "actual:~s expected:~s\n" actual expected)
        (assert (equal? actual expected)))))))

(test
  (lambda (p)
    `(system
       [(init main '()) (output-port ,p)]
       (define (main)
         [_ (output (state))])))
  '(('())))

(test
  (lambda (p)
    `(system
       [(init main '()) (output-port ,p)]
       (define (main)
         [_ (seq
              (output (value 1))
              (output (value 2)))])))
  '((1) (2)))
