(import (chezscheme))

(load "silly-actor.scm")
(load "c-backend.scm")

(define passes '(parse-Lsrc
                 list-to-cons+nil
                 tag-values
                 desugar
                 continuation-constructs
                 to-monad
                 match-conts))

(define (compile x)
  (fold-left (lambda (x f) ((eval f) x)) x passes))

(define (interpret x)
  (let ([code (output-scheme (compile x))])
    (eval code (environment '(scheme) '(runtime)))))

(define utf-8-transcoder
  (make-transcoder (utf-8-codec)))

(define (read-objects p)
  (reverse (let go ([acc '()])
             (let ([o (read p)])
               (cond
                 [(eof-object? o) acc]
                 [else (go (cons o acc))])))))

(define (read-lines p)
  (let go ([acc ""])
    (let ([l (get-line p)])
      (cond
        [(eof-object? l) acc]
        [else (go (string-append acc l "\n"))]))))

(define (run-c code output)
  (pretty-print
    (unparse-Lstack
      (to-stack
        (de-bruijn
          (compile code)))))
  (let ([c-code (output-c (de-bruijn (compile code)))])
    (assert (c-backend c-code (gcc-with-output output)))
    (let-values ([(to-stdin from-stdout from-stderr pid)
                  (open-process-ports output 'line utf-8-transcoder)])
      (let ([os (read-objects from-stdout)]
            [err (read-lines from-stderr)]
            [ec (wait-for-pid pid)])
        (display err)
        (assert (eq? ec 0))
        os))))

(define-record-type test-case (fields name source expected output))

(define (run-test tc)
  (let* ([str (call-with-string-output-port
                (interpret (test-case-source tc)))]
         [ac (run-c (test-case-source tc) (test-case-output tc))]
         [a (with-input-from-string str
              (lambda () (read-objects (current-input-port))))])
    (log-info "tc:~s actual:~a actual-c:~a expected:~a"
              (test-case-name tc)
              a ac
              (test-case-expected tc))
    (assert (equal? a (test-case-expected tc)))
    (assert (equal? ac (test-case-expected tc)))
    ))

(define (load-sexp fn) (call-with-input-file fn read-objects))

(define (test-cases d)
  (sort (lambda (a b) (string<? (test-case-name a) (test-case-name b)))
        (map (lambda (fn)
               (let* ([n (path-last (path-root fn))]
                      [s (car (load-sexp (string-append d "/" n ".sa")))]
                      [e (load-sexp (string-append d "/" n ".expected"))]
                      [o (string-append d "/" n ".exe")])
                 (make-test-case n s e o)))
             (filter (lambda (fn) (equal? (path-extension fn) "sa"))
                     (directory-list d)))))

(load "readme.scm")

(pretty-print (language->s-expression Lstack))

(let ([tcs (test-cases "examples")])
  (for-all run-test tcs)
  (generate-readme tcs))
