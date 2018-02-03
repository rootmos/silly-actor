(import (matchable) (nanopass))

(define primfuns
  '((stop 0 stopM)
    (spawn 2 spawnM)
    (send 2 sendM)
    (become 2 becomeM)
    (stay 1 stayM)
    (from 0 fromM)
    (msg 0 msgM)
    (parent 0 parentM)
    (state 0 stateM)
    (self 0 selfM)
    (output 1 outputM)
    ))

(define (primfun? x) (list? (member x (map car primfuns))))
(define (monadfun? x) (list? (member x (map caddr primfuns))))

(define (atom? x)
  (and
    [symbol? x]
    [not (primfun? x)]
    [char-lower-case? (car (string->list (symbol->string x)))]
    [not (equal? (substring (symbol->string x) 0 1) "_")]
    ))
(define (anf-val? x)
  (and
    [symbol? x]
    [>= (string-length (symbol->string x)) 5]
    [equal? (substring (symbol->string x) 0 5) "_anf-"]
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
    '()
    (list v* ...))
  (Expr (e)
    (value v)
    (var a)
    (pf)
    (pf e* ...)
    (seq e* ...)
    (list e* ...)
    (actor ma* ...)
    (match e ma* ...))
  (MatchArm (ma) (p e))
  (ActorDef (ad) (define (a) ma* ...))
  (Options (o)
    (init a v)
    (output-port op))
  (System (t) (system (o* ...) ad* ...)))

(define-parser parse-Lsrc Lsrc)

(define-language
  L1-monad
  (extends Lsrc)
  (terminals
    (- (primfun (pf)))
    (+ (monadfun (mf)))
    (+ (anf-val (av))))
  (Value (v)
    (+ (anf-val av)))
  (ActorDef (ad)
    (- (define (a) ma* ...))
    (+ (define (a) e)))
  (Expr (e)
    (- (actor ma* ...))
    (+ (close e))
    (- (value v))
    (+ (point v))
    (+ (>> e0 e1))
    (+ (>>= e0 (av) e1))
    (- (pf))
    (+ mf)
    (- (pf e* ...))
    (+ (mf v* ...))
    (- (seq e* ...))
    (- (list e* ...))
    (- (match e ma* ...))
    (+ (match v ma* ...))))

(define-pass to-monad : Lsrc (l) -> L1-monad ()
  (definitions
    (define anf-counter 0)
    (define (fresh-anf-var)
      (let* ([c anf-counter]
             [a (string->symbol (format "_anf-~s" c))])
        (set! anf-counter (+ c 1)) a))
    (with-output-language (L1-monad Value)
      (define (mk-anf-val av) `(anf-val ,av)))
    (with-output-language (L1-monad Expr)
      (define (mk-actor ma*)
        (let ([av (fresh-anf-var)])
          `(>>= msgM (,av)
                (match
                  ,(mk-anf-val av)
                  ,(map MatchArm ma*)
                  ...)))))
    )
  (MatchArm : MatchArm (ma) -> MatchArm ())
  (Value : Value (v) -> Value ())
  (ActorDef : ActorDef (ad) -> ActorDef ()
    [(define (,a) ,ma* ...) `(define (,a) ,(mk-actor ma*))])
  (Expr : Expr (e) -> Expr ()
    [(value ,v) `(point ,(Value v))]
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
                (let mk-anf ([es e*] [vs '()])
                  (cond
                    [(null? es) `(,(caddr l) ,(map mk-anf-val (reverse vs)) ...)]
                    [else
                      (let ([a (fresh-anf-var)])
                        `(>>= ,(Expr (car es)) (,a)
                              ,(mk-anf (cdr es) (cons a vs))))]))]
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
    [(actor ,ma* ...) `(close ,(mk-actor ma*))]
    [(match ,e ,ma* ...)
     (let ([a (fresh-anf-var)])
       `(>>= ,(Expr e) (,a) (match ,(mk-anf-val a) ,(map MatchArm ma*) ...)))]
    ))

(define-pass output-scheme : L1-monad (l) -> * ()
  (Pattern : Pattern (p) -> * ()
    [(atom ,a) `'(atom . ,a)]
    [(number ,n) `'(number . ,n)]
    [(var ,a) `'(var . ,a)]
    [,wp ''wildcard]
    ['() ''()]
    [(list ,p* ...) (cons 'list (map Pattern p*))])
  (Value : Value (v) -> * ()
    [(atom ,a) `'(atom . ,a)]
    [(anf-val ,av) av]
    [(number ,n) `'(number . ,n)]
    ['() ''()]
    [(list ,v* ...) (cons 'list (map Value v*))])
  (Expr : Expr (e) -> * ()
    [(point ,v) (list 'point (Value v))]
    [(match ,v ,ma* ...)
     (list 'matchM (Value v) (cons 'list (map MatchArm ma*)))]
    [(,mf ,v* ...) (cons mf (map Value v*))]
    [,mf mf]
    [(>>= ,e0 (,av) ,e1) `(>>= ,(Expr e0) (lambda (,av) ,(Expr e1)))]
    [(>> ,e0 ,e1) `(>> ,(Expr e0) ,(Expr e1))]
    [(var ,a) `(lookupM ',a)]
    [(close ,e) `(closeM ,(Expr e))])
  (MatchArm : MatchArm (ma) -> * ()
    [(,p ,e) `(cons ,(Pattern p) ,(Expr e))])
  (ActorDef : ActorDef (ad) -> * ()
    [(define (,a) ,e) `(cons ',a ,(Expr e))])
  (Options : Options (o) -> * ()
    [(init ,a ,v) `(init ,a ,(Value v))]
    [(output-port ,op) `(output-port ,op)])
  (System : System (s) -> * ()
    [(system (,o* ...) ,ad* ...)
     `(run-system
        ',(map Options o*)
        ,(cons 'list  (map ActorDef ad*)))]))
