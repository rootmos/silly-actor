(load "silly-actor.scm")

(define (interp x)
  (let ([code (output-scheme (parse-Lsrc x))])
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
