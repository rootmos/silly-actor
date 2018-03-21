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
        [else (go (string-append acc l "\n" ))]))))

(define (run-c code)
  (let ([c-code (output-c (to-stack (compile code)))])
    (assert (c-backend c-code gcc-a-out))
    (let-values ([(to-stdin from-stdout from-stderr pid)
                  (open-process-ports "./a.out" 'line utf-8-transcoder)])
      (let ([os (read-objects from-stdout)]
            [err (read-lines from-stderr)]
            [ec (wait-for-pid pid)])
        (close-port to-stdin) (close-port from-stdout)
        (close-port from-stderr)
        (display err)
        (assert (eq? ec 0))
        os))))

(define (test code expected)
  (let* ([str (call-with-string-output-port (interpret code))]
         [ac (run-c code)]
         [a (with-input-from-string str
              (lambda () (read-objects (current-input-port))))])
    (log-info "actual:~a actual-c:~a expected:~a" a ac expected)
    (assert (equal? a expected))
    (assert (equal? ac expected))
    ))

(test
  '(system
     [(init Main ())]
       (define (Main) [Init (output (state))]))
  '(()))

(test
  '(system
     [(init Main ())]
     (define (Main) [_ (seq (output 1) (output 2))]))
  '((number . 1) (number . 2)))

(test
  '(system
     [(init Main 7)]
     (define (Main) [_ (output (state))]))
  '((number . 7)))

(test
  '(system
     [(init Main ())]
     (define (Main) [8 (output success)] [_ (send (self) 8)]))
  '((atom . success)))

(test
  '(system
     [(init Main ())]
       (define (Main)
         [_ (seq
              (send (self) 8)
              (become (actor [8 (output success)]) (state)))]))
  '((atom . success)))

(test
  '(system
     [(init Main ())]
     (define (Aux) [7 (output (state))] [_ ()])
     (define (Main) [_ (let ([Id (spawn Aux success)]) (send Id 7))]))
  '((atom . success)))

(test
  '(system
     [(init Main ())]
     (define (Aux) [7 (output (state))] [_ ()])
     (define (Main) [_ (send (spawn Aux success) 7)]))
  '((atom . success)))

(test
  '(system
     [(init Main ())]
     (define (Main) [_ (let ([X 7]) (match 7 ['X (output success)]))]))
  '((atom . success)))

(test
  '(system
     [(init Main ())]
     (define (Main) [_ (match '(1 2) [(X _) (output X)])]))
  '((number . 1)))

(test
  '(system
     [(init Main ())]
     (define (Aux) [7 (reply 8)] [_ ()])
     (define (Main)
       [_ (seq (send (spawn Aux ()) 7) (output (recv [8 success])))]))
  '((atom . success)))

(test
  '(system
     [(init Main ())]
     (define (Aux) [7 (seq (reply 8) (reply 13))] [9 (reply 10)] [_ ()])
     (define (Main)
       [_ (let ([Id (spawn Aux ())])
            (seq
              (output (call Id 7 [8 a]))
              (output (call Id 9 [10 b]))))]))
  '((atom . a) (atom . b)))
