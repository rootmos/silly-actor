(load "silly-actor.scm")

(define (interp x)
  (let ([code (output-scheme
                (to-monad
                  (desugar
                    (parse-Lsrc x))))])
    (pretty-print code)
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
  '('()))

(test
  (lambda (p)
    `(system
       [(init main '()) (output-port ,p)]
       (define (main)
         [_ (seq
              (output (value (number 1)))
              (output (value (number 2))))])))
  '((number . 1) (number . 2)))

(test
  (lambda (p)
    `(system
       [(init main (number 7)) (output-port ,p)]
       (define (main) [_ (output (state))])))
  '('(number . 7)))

(test
  (lambda (p)
    `(system
       [(init main '()) (output-port ,p)]
       (define (main)
         [(number 8) (output (value (atom success)))]
         [_ (send (self) (value (number 8)))])))
  '((atom . success)))

(test
  (lambda (p)
    `(system
       [(init main '()) (output-port ,p)]
       (define (main)
         [_ (seq
              (send (self) (value (number 8)))
              (become (actor [(number 8) (output (value (atom success)))])
                      (state)))])))
  '((atom . success)))

(test
  (lambda (p)
    `(system
       [(init main '()) (output-port ,p)]
       (define (aux)
         [(number 7) (output (state))]
         [_ (value '())])
       (define (main)
         [_ (let ([(var id) (spawn (var aux) (value (atom success)))])
              (send (var id) (value (number 7))))])))
  '((atom . success)))

(test
  (lambda (p)
    `(system
       [(init main '()) (output-port ,p)]
       (define (aux)
         [(number 7) (output (state))]
         [_ (value '())])
       (define (main)
         [_ (send (spawn (var aux) (value (atom success)))
                                   (value (number 7)))])))
  '((atom . success)))
