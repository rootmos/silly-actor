(import (matchable) (nanopass))

(define verbs
  '(match stop spawn send become actor stay self from parent))

(define (verb? x) (list? (member x verbs)))
(define (atom? x)
  (and
    [symbol? x]
    [not (verb? x)]
    [char-lower-case? (car (string->list (symbol->string x)))]
    [not (equal? (substring (symbol->string x) 0 1) "_")]
    ))
(define (variable-pattern? x) (match x [('unquote s) (atom? s)] [else #f]))
(define (wildcard-pattern? x)
  (and
    [symbol? x]
    [equal? (substring (symbol->string x) 0 1) "_"]
    ))

(define-language
  Lsrc
  (terminals
    (atom (a))
    (number (n))
    (variable-pattern (vp))
    (wildcard-pattern (wp)))
  (entry System)
  (Pattern (p)
    a
    n
    vp
    wp
    '()
    (p* ...))
  (Expr (e)
    a
    n
    ua
    (become e0 e1)
    (stay e)
    (spawn e0 e1)
    (send e0 e1)
    (stop)
    (self)
    '()
    (e* ...))
  (MsgCase (m) (p e))
  (UnnamedActor (ua) (actor (p) m* ...))
  (ActorDef (ad) (define (a p) m* ...))
  (Options (o) (init a))
  (System (t) (system (o* ...) ad* ...)))

(define-parser parse-Lsrc Lsrc)

(define-pass output-scheme : Lsrc (e) -> * ()
  (ActorDef : ActorDef (ad ds) -> * ()
    [(define (,a ,p) ,m* ...)
     (hashtable-set! ds a
       (lambda (self parent state from msg)
         (printf "self:~s parent:~s state:~s from:~s: msg:~s\n"
                 self parent state from msg)
         ))])
  (Options : Options (o) -> * ()
    [(init ,a) `(init ,a)])
  (System : System (s) -> * ()
    [(system (,o* ...) ,ad* ...)
     (let ([ads (make-eqv-hashtable)])
       (for-each (lambda (ad) (ActorDef ad ads)) ad*)
       (make-system
         (map Options o*)
         (empty-queue)
         ads
         (make-eqv-hashtable)
         (box 0)))]))

(define-record-type system
  (fields options inbox actor-defs actors actor-counter))
(define-record-type actor (fields parent f state))

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

(define (run-system s)
  (let*
    ([get-actor-def
       (lambda (a) (hashtable-ref (system-actor-defs s) a (void)))]
     [fresh-actor-id
       (lambda () (let* ([b (system-actor-counter s)]
                         [c (unbox b)])
                    (set-box! b (+ c 1)) c))]
     [send-message
       (lambda (id from msg) (enqueue (system-inbox s) `(,id ,from ,msg)))]
     [deliver-message
       (lambda ()
         (match (dequeue (system-inbox s))
           [(id from msg)
            (let ([a (hashtable-ref (system-actors s) id (void))])
              ((actor-f a) id (actor-parent a) (actor-state a) from msg))]))]
     [spawn-actor (lambda (p id f st)
                    (hashtable-set! (system-actors s) id (make-actor p f st))
                    (send-message id 'root 'Init))]
     )
    (for-each (lambda (o)
                (match o
                  [('init a)
                   (spawn-actor
                     'root
                     (fresh-actor-id)
                     (get-actor-def a)
                     'TODO)]
                  [else (void)]))
              (system-options s))
    (call/cc (lambda (k)
               (let go ()
                 (if (queue-empty? (system-inbox s)) (k)
                   (begin
                     (deliver-message)
                     (go))))))
    (printf "system stopped\n")))
