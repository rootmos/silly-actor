(load "silly-actor.scm")
(load "c-backend.scm")

(define passes '(parse-Lsrc
                 list-to-cons+nil
                 tag-values
                 desugar
                 continuation-constructs
                 to-monad))

(define (compile x)
  (fold-left (lambda (x f) ((eval f) x)) x passes))

(define (interp x)
  (pretty-print x)
  (let ([code (output-scheme (compile x))])
    (eval code (environment '(scheme) '(runtime)))))

(define (run-c code)
  (let ([c-code (output-c (to-stack (compile code)))])
    (assert (c-backend c-code gcc-a-out))
    (assert (eq? (system "./a.out") 0))
    ))

(define (test code expected)
  (run-c (code (current-output-port)))
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
       [(init Main ()) (output-port ,p)]
       (define (Main) [Init (output (state))])))
  '('()))

(test
  (lambda (p)
    `(system
       [(init Main ()) (output-port ,p)]
       (define (Main) [_ (seq (output 1) (output 2))])))
  '((number . 1) (number . 2)))

(test
  (lambda (p)
    `(system
       [(init Main 7) (output-port ,p)]
       (define (Main) [_ (output (state))])))
  '('(number . 7)))

(test
  (lambda (p)
    `(system
       [(init Main ()) (output-port ,p)]
       (define (Main) [8 (output success)] [_ (send (self) 8)])))
  '((atom . success)))

(test
  (lambda (p)
    `(system
       [(init Main ()) (output-port ,p)]
       (define (Main)
         [_ (seq
              (send (self) 8)
              (become (actor [8 (output success)]) (state)))])))
  '((atom . success)))

(test
  (lambda (p)
    `(system
       [(init Main ()) (output-port ,p)]
       (define (Aux) [7 (output (state))] [_ ()])
       (define (Main) [_ (let ([Id (spawn Aux success)]) (send Id 7))])))
  '((atom . success)))

(test
  (lambda (p)
    `(system
       [(init Main ()) (output-port ,p)]
       (define (Aux) [7 (output (state))] [_ ()])
       (define (Main) [_ (send (spawn Aux success) 7)])))
  '((atom . success)))

(test
  (lambda (p)
    `(system
       [(init Main ()) (output-port ,p)]
       (define (Main) [_ (let ([X 7]) (match 7 ['X (output success)]))])))
  '((atom . success)))

(test
  (lambda (p)
    `(system
       [(init Main ()) (output-port ,p)]
       (define (Main) [_ (match '(1 2) [(X _) (output X)])])))
  '((number . 1)))

(test
  (lambda (p)
    `(system
       [(init Main ()) (output-port ,p)]
       (define (Aux) [7 (reply 8)] [_ ()])
       (define (Main)
         [_ (seq (send (spawn Aux ()) 7) (output (recv [8 success])))])))
  '((atom . success)))

(test
  (lambda (p)
    `(system
       [(init Main ()) (output-port ,p)]
       (define (Aux) [7 (seq (reply 8) (reply 13))] [9 (reply 10)] [_ ()])
       (define (Main)
         [_ (let ([Id (spawn Aux ())])
              (seq
                (output (call Id 7 [8 a]))
                (output (call Id 9 [10 b]))))])))
  '((atom . a) (atom . b)))
