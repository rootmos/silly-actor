(library (runtime)
  (export pattern-match
          actor-state actor-id
          run-system)
  (import (chezscheme))

  (define-record-type as
                      (fields inbox actors actor-counter root-env))
  (define-record-type actor
                      (fields id parent-id f state system))
  
  (define (ni what) (error what "not implemented"))
  
  (define-record-type queue (fields (mutable head) (mutable tail)))
  (define (empty-queue) (let ([q (box '())]) (make-queue q q)))
  (define (enqueue q x)
    (let ([t (queue-tail q)] [nt (box '())])
      (set-box! t `(,x . ,nt))
      (queue-tail-set! q nt)))
  (define (dequeue q)
    (let* ([hb (queue-head q)] [h (unbox hb)] )
      (case h
        ['() (error 'dequeue "empty queue" q)]
        [else (queue-head-set! q (cdr h)) (car h)])))
  (define (queue-empty? q) (null? (unbox (queue-head q))))

  (define (lookup a env)
    (cond
      [(assv a env) => (lambda (v) (cdr v))]
      [else (error 'lookup "unbound" a)]))

  (define (fresh-actor-id s)
    (let* ([b (as-actor-counter s)]
           [c (unbox b)])
      (set-box! b (+ c 1)) c))

  (define (send-message a to msg)
    (let ([s (actor-system a)]
          [from (actor-id a)])
      (enqueue (as-inbox s)
               `(,to ,from ,msg))))

  (define (deliver-message s)
    (let* ([m (dequeue (as-inbox s))]
           [to (car m)] [from (cadr m)] [msg (caddr m)]
           [a (hashtable-ref (as-actors s) to (void))])
      ((actor-f a) a from msg)))

  (define (spawn-actor a f st)
    (let* ([s (actor-system a)]
           [id (fresh-actor-id s)])
      (hashtable-set!
        (as-actors s)
        id
        (make-actor id (actor-id a) f st s))
      (send-message a id 'Init)))

  (define (pattern-match x ps)
    (void))

  (define (run-system os root-env)
    (let* ([s (make-as (empty-queue) (make-eqv-hashtable) (box 0) root-env)]
           [root (make-actor 'root 'root (lambda () (void)) '() s)])
      (for-each (lambda (o)
                  (cond
                    [(and (list? o) (not (null? o)) (eqv? (car o) 'init))
                     (let ([a (cadr o)] [v (caddr o)])
                       (spawn-actor
                         root
                         (lookup a root-env)
                         v))]
                    [else (void)])) os)
      (call/cc (lambda (k)
                 (let go ()
                   (if (queue-empty? (as-inbox s)) (k)
                     (begin
                       (deliver-message s)
                       (go))))))
      (printf "actor system stopped\n")))
  )
