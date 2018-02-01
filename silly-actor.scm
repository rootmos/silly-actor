(import (matchable) (nanopass))

(define verbs
  '(match stop spawn send become actor stay from parent list state self output))

(define (verb? x) (list? (member x verbs)))
(define (atom? x)
  (and
    [symbol? x]
    [not (verb? x)]
    [char-lower-case? (car (string->list (symbol->string x)))]
    [not (equal? (substring (symbol->string x) 0 1) "_")]
    ))
(define (variable-pattern? x)
  (match x [('unquote s) (atom? s)] [else #f]))
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
    (wildcard-pattern (wp))
    (output-port (op)))
  (entry System)
  (Pattern (p)
    (atom a)
    n
    (var a)
    wp
    '()
    (p* ...))
  (Value (v)
    (atom a)
    n
    (actor ma* ...)
    '()
    (v* ...))
  (Expr (e)
    (value v)
    (var a)
    (become e0 e1)
    (stay e)
    (spawn e0 e1)
    (send e0 e1)
    (stop)
    (self)
    (from)
    (state)
    (output e)
    (list e* ...))
  (MatchArm (ma) (p e))
  (ActorDef (ad) (define (a) ma* ...))
  (Options (o)
    (init a v)
    (output-port op))
  (System (t) (system (o* ...) ad* ...)))

(define-parser parse-Lsrc Lsrc)

(define-pass output-scheme : Lsrc (e) -> * ()
  (definitions
    (define anf-counter 0)
    (define fresh-anf-var
      (lambda ()
        (let* ([c anf-counter]
               [s (string->symbol (format "_anf-~s" c))])
          (set! anf-counter (+ c 1))
          s)))
    (define mk-actor
      (lambda (ma*)
        `(>>= msgM (pattern-matchM ,(cons 'list (map MatchArm ma*))))))
    )
  (Value : Value (v) -> * ()
    [(atom ,a) `'(atom . ,a)]
    [,n n]
    ['() ''()]
    [(actor ,ma* ...) (mk-actor ma*)]
    [(,v* ...) (cons 'list (map Value v*))])
  (Pattern : Pattern (p) -> * ()
    [(atom ,a) `'(atom . ,a)]
    [,n `,n]
    ['() ''()]
    [(var ,a) `'(var . ,a)]
    [,wp ''wildcard]
    [(,p* ...) (cons 'list (map Pattern p*))])
  (Expr : Expr (e) -> * ()
    [(value ,v) (list 'point (Value v))]
    [(var ,a) `(lookupM ',a)]
    [(become ,e0 ,e1)
     (let ([v0 (fresh-anf-var)] [v1 (fresh-anf-var)])
       `(>>= ,(Expr e0)
             (lambda (,v0)
               (>>= ,(Expr e1)
                    (lambda (,v1)
                      (becomeM ,v0 ,v1))))))]
    [(stay ,e)
     (let ([v0 (fresh-anf-var)])
       `(>>= ,(Expr e) stayM))]
    [(spawn ,e0 ,e1)
     (let ([v0 (fresh-anf-var)] [v1 (fresh-anf-var)])
       `(>>= ,(Expr e0)
             (lambda (,v0)
               (>>= ,(Expr e1)
                    (lambda (,v1)
                      (spawnM ,v0 ,v1))))))]
    [(send ,e0 ,e1)
     (let ([v0 (fresh-anf-var)] [v1 (fresh-anf-var)])
       `(>>= ,(Expr e0)
             (lambda (,v0)
               (>>= ,(Expr e1)
                    (lambda (,v1)
                      (sendM ,v0 ,v1))))))]
    [(output ,e)
     (let ([v (fresh-anf-var)])
       `(>>= ,(Expr e) (lambda (,v) (sendM 'output ,v))))]
    [(stop) 'stopM]
    [(from) 'fromM]
    [(self) 'selfM]
    [(state) 'stateM]
    [(list ,e* ...) (cons 'list (map Expr e*))])
  (MatchArm : MatchArm (ma) -> * ()
    [(,p ,e) `(cons ,(Pattern p) ,(Expr e))])
  (ActorDef : ActorDef (ad) -> * ()
    [(define (,a) ,ma* ...) `(cons ',a ,(mk-actor ma*))])
  (Options : Options (o) -> * ()
    [(init ,a ,v) `(init ,a ,(Value v))]
    [(output-port ,op) `(output-port ,op)])
  (System : System (s) -> * ()
    [(system (,o* ...) ,ad* ...)
     `(run-system
        ',(map Options o*)
        ,(cons 'list  (map ActorDef (reverse ad*))))]))
