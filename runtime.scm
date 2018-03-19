(library (runtime)
  (export >>= point
          stateM msgM fromM selfM set-stateM
          lookupM matchM closeM
          becomeM spawnM sendM stopM
          with/ccM continueM yieldM
          run-system)
  (import (chezscheme))

  (define-record-type as (fields inbox actors actor-counter root-env))
  (define-record-type cl (fields f env))
  (define-record-type a
    (fields id parent-id (mutable cl) (mutable state) system))

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
    (let ([s (a-system a)]
          [from (a-id a)])
      (enqueue (as-inbox s)
               `(,to ,from ,msg))))

  (define (deliver-message s)
    (let* ([m (dequeue (as-inbox s))]
           [to (car m)] [from (cadr m)] [msg (caddr m)])
      (cond
        [(hashtable-ref (as-actors s) to #f) => (lambda (a)
           ;(printf "delivering message:~s to:~s from:~s\n" msg to from)
           (call/cc (lambda (k)
                      ((cl-f (a-cl a)) (make-ctx a k from msg)
                                       (cl-env (a-cl a))))))]
        [else (printf "undeliverable message:~s to:~s from:~s\n" msg to from)]
        )))

  (define (spawn-actor a cl st)
    (let* ([s (a-system a)] [id (fresh-actor-id s)])
      (hashtable-set! (as-actors s) id (make-a id (a-id a) cl st s))
      (send-message a id '(sys . Init))
      id))

  (define (remove-actor a) ; TODO make more efficient!
    (let ([id (a-id a)] [as (as-actors (a-system a))])
      (hashtable-delete! as (a-id a))
      (let-values ([(is xs) (hashtable-entries as)])
        (vector-for-each
          (lambda (a) (if (eqv? id (a-parent-id a)) (remove-actor a)))
          xs))))

  (define-record-type ctx (fields a k from msg))

  (define stateM (lambda (ctx env) (values (a-state (ctx-a ctx)) env)))
  (define msgM (lambda (ctx env) (values (ctx-msg ctx) env)))
  (define fromM (lambda (ctx env) (values (ctx-from ctx) env)))
  (define selfM (lambda (ctx env) (values (a-id (ctx-a ctx)) env)))

  (define (with/ccM kv m)
    (lambda (ctx env)
      (values (call/cc (lambda (k) (m ctx (cons (cons kv k) env)))) env)))
  (define (continueM kv v) (lambda (ctx env) ((lookup kv env) v)))

  (define (die ctx e)
    (let* ([a (ctx-a ctx)] [to (a-parent-id a)])
      (send-message a to (cons '(sys . Died) e))
      (remove-actor a)
      ((ctx-k ctx))))

  (define (lookupM a) (lambda (ctx env) (values (lookup a env) env)))

  (define (>>= ma f) (lambda (ctx env)
                       (let-values ([(x env_) (ma ctx env)])
                         ((f x) ctx env))))
  (define (point x) (lambda (ctx env) (values x env)))

  (define (match x p env)
    (cond
      [(eqv? 'wildcard p) env]
      [(and (pair? p) (pair? x)
            (eqv? (car p) (car x))
            (eqv? (cdr p) (cdr x))) env]
      [(and (eqv? p '()) (eqv? x '())) env]
      [(and (pair? p) (eqv? (car p) 'bind)) (cons (cons (cdr p) x) env)]
      [(and (pair? p) (eqv? (car p) 'var))
       (cond [(equal? x (lookup (cdr p) env)) env]
             [else #f])]
      [(and (list? p) (eqv? (car p) 'cons)
            (list? x) (eqv? (car x) 'cons))
       (cond
         [(match (cadr x) (cadr p) env) =>
          (lambda (e) (match (caddr x) (caddr p) e))]
         [else #f])]
      [else #f]))

  (define (matchM x ps)
    (lambda (ctx env)
      (let go ([qs ps])
        (cond
          [(null? qs) (die ctx `(Match_error . ,x))]
          [(match x (caar qs) env) => (lambda (bs)
                                        ((cdar qs) ctx (append bs env)))]
          [else (go (cdr qs))]))))

  (define (closeM m) (lambda (ctx env) (values (make-cl m env) env)))

  (define yieldM (lambda (ctx env) ((ctx-k ctx))))

  (define (becomeM cl)
    (lambda (ctx env)
      (begin (a-cl-set! (ctx-a ctx) cl) (yieldM ctx env))))

  (define (set-stateM st)
    (lambda (ctx env)
      (begin (a-state-set! (ctx-a ctx) st) (values '() env))))

  (define (spawnM cl st)
    (lambda (ctx env)
      (values (spawn-actor (ctx-a ctx) cl st) env)))

  (define (sendM id m)
    (lambda (ctx env)
      (send-message (ctx-a ctx) id m)
      (values '() env)))

  (define stopM
    (lambda (ctx env)
      (let* ([a (ctx-a ctx)] [to (a-parent-id a)] [s (a-system a)])
        (send-message a to '(sys . Stopped))
        (remove-actor a)
        (yieldM ctx env))))

  (define (run-system os root-defs)
    (let* ([root-env (fold-left (lambda (env d)
                                  (cons (cons (car d) (make-cl (cdr d) env))
                                        env))
                                '()
                                root-defs)]
           [s (make-as (empty-queue) (make-eqv-hashtable) (box 0) root-env)]
           [root (make-a 'Root
                         'Root
                         (make-cl (lambda (ctx env) (void)) root-env)
                         '()
                         s)]
           [sys-actor (lambda (id f)
                        (hashtable-set!
                          (as-actors s) id
                          (make-a id 'Root (make-cl f root-env) '() s)))]
           [output-port (current-output-port)])
      (hashtable-set! (as-actors s) 'Root root)

      (sys-actor 'Output
                 (lambda (ctx env)
                   (write (ctx-msg ctx) output-port)))

      (for-each (lambda (o)
                  (cond
                    [(and (eqv? (car o) 'init))
                     (let ([a (cadr o)] [v (caddr o)])
                       (spawn-actor
                         (hashtable-ref (as-actors s) 'Root #f)
                         (lookup a root-env)
                         v))]
                    [(and (eqv? (car o) 'output-port))
                     (set! output-port (cadr o))]
                    [else (void)])) os)
      (call/cc (lambda (k)
                 (let go ()
                   (if (queue-empty? (as-inbox s)) (k)
                     (begin
                       (deliver-message s)
                       (go))))))
      (printf "actor system stopped\n")))
  )
