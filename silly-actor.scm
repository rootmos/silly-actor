(import (matchable) (nanopass))

(define (fun? x)
  (list? (member x '(stop spawn send become stay
                          from msg parent state self output reply))))

(define (atom? x)
  (and
    [symbol? x]
    [not (fun? x)]
    [char-lower-case? (car (string->list (symbol->string x)))]
    [not (equal? (substring (symbol->string x) 0 1) "_")]
    ))

(define (sys? x)
  (list? (member x '(Init Match_error Stopped Output))))

(define (bound-var? x)
  (and
    [symbol? x]
    [not (sys? x)]
    [char-upper-case? (car (string->list (symbol->string x)))]
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
    (sys (s))
    (number (n))
    (wildcard-pattern (wp))
    (fun (f))
    (bound-var (bv))
    (output-port (op)))
  (entry System)
  (Pattern (p)
    a
    s
    n
    bv
    (quote bv)
    wp
    '()
    (list p* ...))
  (Value (v)
    a
    n
    '()
    (list v* ...))
  (Expr (e)
    v
    bv
    (f)
    (f e* ...)
    (seq e* ...)
    (list e* ...)
    (actor ma* ...)
    (match e ma* ...)
    (let (ma* ...) e))
  (MatchArm (ma) (p e))
  (ActorDef (ad) (define (bv) ma* ...))
  (Options (o)
    (init bv v)
    (output-port op))
  (System (t) (system (o* ...) ad* ...)))

(define-language
  Ltagged
  (extends Lsrc)
  (Pattern (p)
    (- a)
    (+ (atom a))
    (- n)
    (+ (number n))
    (- s)
    (+ (sys s))
    (- bv)
    (+ (bind bv))
    (- (quote bv))
    (+ (var bv)))
  (Value (v)
    (- a)
    (+ (atom a))
    (- n)
    (+ (number n)))
  (Expr (e)
    (- v)
    (+ (value v))
    (- bv)
    (+ (var bv))))

(define-pass tag-values : Lsrc (l) -> Ltagged ()
  (Pattern : Pattern (v) -> Pattern ()
    [,n `(number ,n)]
    [,a `(atom ,a)]
    [,s `(sys ,s)]
    [,bv `(bind ,bv)]
    [(quote ,bv) `(var ,bv)])
  (Value : Value (v) -> Value ()
    [,n `(number ,n)]
    [,a `(atom ,a)])
  (Expr : Expr (e) -> Expr ()
    [,v `(value ,(Value v))]
    [,bv `(var ,bv)])
  )

(define-parser parse-Lsrc Lsrc)

(define (primfun? x)
  (list? (member x '(stop spawn send become from msg parent state
                          self this-closure))))

(define-language
  Ldesugared
  (extends Ltagged)
  (terminals
    (- (fun (f)))
    (+ (primfun (pf))))
  (Value (v)
    (+ (sys s)))
  (Expr (e)
    (- (f))
    (+ (pf))
    (- (f e* ...))
    (+ (pf e* ...))
    (- (let (ma* ...) e))))

(define-pass desugar : Ltagged (l) -> Ldesugared ()
  (Pattern : Pattern (p) -> Pattern ())
  (split-arm : MatchArm (ma) -> * ()
    [(,p ,e) (cons p e)])
  (Expr : Expr (e) -> Expr ()
    [(,f) `(,f)]
    [(,f ,e* ...)
     (case f
       ['stay `(become (this-closure) ,(map Expr e*) ...)]
       ['output `(send (value (sys Output)) ,(map Expr e*) ...)]
       ['reply `(send (from) ,(map Expr e*) ...)]
       [else `(,f ,(map Expr e*) ...)])]
    [(let (,ma* ...) ,e)
     (let go ([mas ma*])
       (cond
         [(null? mas) (Expr e)]
         [else
           (let ([arm (split-arm (car mas))])
           `(match ,(Expr (cdr arm)) (,(Pattern (car arm)) ,(go (cdr mas)))))]))]
    ))

(define primfun-to-monadfun
  '((stop 0 stopM)
    (spawn 2 spawnM)
    (send 2 sendM)
    (become 2 becomeM)
    (from 0 fromM)
    (msg 0 msgM)
    (parent 0 parentM)
    (state 0 stateM)
    (self 0 selfM)
    (this-closure 0 this-closureM)
    ))

(define (monadfun? x)
  (list? (member x '(stopM spawnM sendM becomeM
                           fromM msgM parentM stateM selfM this-closureM))))

(define-language
  Lmonad
  (extends Ldesugared)
  (terminals
    (- (primfun (pf)))
    (+ (monadfun (mf)))
    (+ (anf-val (av))))
  (Value (v)
    (+ (anf-val av)))
  (ActorDef (ad)
    (- (define (bv) ma* ...))
    (+ (define (bv) e)))
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

(define-pass to-monad : Ldesugared (l) -> Lmonad ()
  (definitions
    (define anf-counter 0)
    (define (fresh-anf-var)
      (let* ([c anf-counter]
             [a (string->symbol (format "_anf-~s" c))])
        (set! anf-counter (+ c 1)) a))
    (with-output-language (Lmonad Value)
      (define (mk-anf-val av) `(anf-val ,av)))
    (with-output-language (Lmonad Expr)
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
    [(define (,bv) ,ma* ...) `(define (,bv) ,(mk-actor ma*))])
  (Expr : Expr (e) -> Expr ()
    [(value ,v) `(point ,(Value v))]
    [(,pf)
     (cond
       [(assv pf primfun-to-monadfun)
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
       [(assv pf primfun-to-monadfun)
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

(define-pass output-scheme : Lmonad (l) -> * ()
  (Pattern : Pattern (p) -> * ()
    [(atom ,a) `'(atom . ,a)]
    [(number ,n) `'(number . ,n)]
    [(bind ,bv) `'(bind . ,bv)]
    [(var ,bv) `'(var . ,bv)]
    [(sys ,s) `'(sys . ,s)]
    [,wp ''wildcard]
    ['() ''()]
    [(list ,p* ...) (cons 'list (map Pattern p*))])
  (Value : Value (v) -> * ()
    [(atom ,a) `'(atom . ,a)]
    [(sys ,s) `',s]
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
    [(var ,bv) `(lookupM ',bv)]
    [(close ,e) `(closeM ,(Expr e))])
  (MatchArm : MatchArm (ma) -> * ()
    [(,p ,e) `(cons ,(Pattern p) ,(Expr e))])
  (ActorDef : ActorDef (ad) -> * ()
    [(define (,bv) ,e) `(cons ',bv ,(Expr e))])
  (Options : Options (o) -> * ()
    [(init ,bv ,v) `(init ,bv ,(Value v))]
    [(output-port ,op) `(output-port ,op)])
  (System : System (s) -> * ()
    [(system (,o* ...) ,ad* ...)
     `(run-system
        ',(map Options o*)
        ,(cons 'list  (map ActorDef ad*)))]))
