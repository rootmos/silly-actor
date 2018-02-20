(load "silly-actor.scm")

(define (interp x)
  (let ([code (output-scheme
                (to-monad
                  (desugar
                    (tag-values
                      (list-to-cons+nil
                        (parse-Lsrc x))))))])
    ;(pretty-print code)
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
