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

(define interpreter-passes '(output-scheme))

(define backend-passes '(de-bruijn
                         to-stack
                         collect-atoms
                         output-c))

(define (compile x second-phase)
  (fold-left (lambda (x f) ((eval f) x)) x (append passes second-phase)))

(define (interpret x)
  (let ([code (compile x interpreter-passes)])
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
  (let ([c-code (compile code backend-passes)])
    (assert (c-backend c-code (gcc-with-output output)))
    (let* ([stdout (format "~a.stdout" output)]
           [stderr (format "~a.stderr" output)]
           [cmd (format "exec ~a 1>~a 2>~a" output stdout stderr)])
      (log-debug "running: ~a" cmd)
      (apply
        (lambda (from-stdout to-stdin pid)
          (close-port to-stdin)
          (close-port from-stdout)
          (let ([ec (wait-for-pid pid)])
            (assert (eq? ec 0))
            (let ([os (with-input-from-file stdout
                        (lambda () (read-objects (current-input-port))))]
                  [err (with-input-from-file stderr
                         (lambda () (read-lines (current-input-port))))])
              (display err) os)))
        (process cmd)))))

(define-record-type test-case (fields name source expected output))

(define (run-test tc)
  (log-info "running test: ~a" (test-case-name tc))
  (let* ([str (call-with-string-output-port (interpret (test-case-source tc)))]
         [ac (run-c (test-case-source tc) (test-case-output tc))]
         [a (with-input-from-string str (lambda () (read-objects (current-input-port))))]
         )
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

(let ([tcs (test-cases "examples")])
  (for-all run-test tcs)
  (generate-readme tcs))
