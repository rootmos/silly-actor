(import (matchable) (nanopass))

(define primfuns
  '((stop 0 stopM)
    (spawn 2 spawnM)
    (send 2 sendM)
    (become 2 becomeM)
    (stay 1 stayM)
    (from 0 fromM)
    (parent 0 parentM)
    (state 0 stateM)
    (self 0 selfM)
    (output 1 outputM)))

(define (primfun? x) (list? (member x (map car primfuns))))

(define (atom? x)
  (and
    [symbol? x]
    [not (primfun? x)]
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
    (primfun (pf))
    (output-port (op)))
  (entry System)
  (Pattern (p)
    (atom a)
    (number n)
    (var a)
    wp
    '()
    (list p* ...))
  (Value (v)
    (atom a)
    (number n)
    (actor ma* ...)
    '()
    (list v* ...))
  (Expr (e)
    (value v)
    (var a)
    (pf)
    (pf e* ...)
    (seq e* ...)
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
    [(number ,n) `'(number . ,n)]
    [(actor ,ma* ...) (mk-actor ma*)]
    ['() ''()]
    [(list ,v* ...) (cons 'list (map Value v*))])
  (Pattern : Pattern (p) -> * ()
    [(atom ,a) `'(atom . ,a)]
    [(number ,n) `'(number . ,n)]
    [(var ,a) `'(var . ,a)]
    [,wp ''wildcard]
    ['() ''()]
    [(list ,p* ...) (cons 'list (map Pattern p*))])
  (Expr : Expr (e) -> * ()
    [(value ,v) (list 'point (Value v))]
    [(var ,a) `(lookupM ',a)]
    [(,pf)
     (cond
       [(assv pf primfuns)
        => (lambda (l)
             (cond
               [(= 0 (cadr l)) (caddr l)]
               [else
                 (error
                   'output-scheme
                   "non-nullary primitive function applied without arguments"
                   pf)]))]
       [else (error 'output-scheme "unsupported nullary primitive function"
                    pf)])]
    [(,pf ,e* ...)
     (cond
       [(assv pf primfuns)
        => (lambda (l)
             (cond
               [(= (length e*) (cadr l))
                (let anf ([es e*] [vs '()])
                  (cond
                    [(null? es) (cons (caddr l) (reverse vs))]
                    [else
                      (let ([v (fresh-anf-var)])
                        `(>>=
                           ,(Expr (car es))
                           (lambda (,v) ,(anf (cdr es) (cons v vs)))))]))]
               [else
                 (error
                   'output-scheme
                   "primitive function applied with wrong number of arguments"
                   pf)]))]
       [else (error 'output-scheme "unsupported primitive function" pf)])]
    [(seq ,e* ...)
     (let go ([es e*])
       (cond
         [(equal? (length es) 1) (Expr (car es))]
         [else `(>> ,(Expr (car es)) ,(go (cdr es)))]))]
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
