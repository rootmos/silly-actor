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
    (null (null))
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
    null
    (p* ...))
  (Value (v)
    a
    n
    null
    '(v* ...)
    (list v* ...))
  (Expr (e)
    v
    bv
    (f)
    (f e* ...)
    (list e* ...)
    (seq e* ...)
    (actor ma* ...)
    (match e ma* ...)
    (recv ma* ...)
    (call e0 e1 ma* ...)
    (let (ma* ...) e))
  (MatchArm (ma) (p e))
  (ActorDef (ad) (define (bv) ma* ...))
  (Options (o)
    (init bv v)
    (output-port op))
  (System (t) (system (o* ...) ad* ...)))

(define-parser parse-Lsrc Lsrc)

(define-language
  Lcons+nil
  (extends Lsrc)
  (Pattern (p)
    (- (p* ...))
    (+ (cons p0 p1)))
  (Value (v)
    (- (list v* ...))
    (- '(v* ...))
    (+ (cons v0 v1)))
  (Expr (e)
    (- (list e* ...))
    (+ (cons e0 e1))))

(define-pass list-to-cons+nil : Lsrc (l) -> Lcons+nil ()
  (definitions
    (with-output-language (Lcons+nil Value)
      (define (mk-v vs)
        (cond
          [(null? vs) '()]
          [else `(cons ,(Value (car vs)) ,(mk-v (cdr vs)))]))))
  (Pattern : Pattern (p) -> Pattern ()
    [(,p* ...)
     (let go ([ps p*])
       (cond
         [(null? ps) '()]
         [else `(cons ,(Pattern (car ps)) ,(go (cdr ps)))]))])
  (Value : Value (v) -> Value ()
    [(list ,v* ...) (mk-v v*)]
    ['(,v* ...) (mk-v v*)])
  )

(define-language
  Ltagged
  (extends Lcons+nil)
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

(define-pass tag-values : Lcons+nil (l) -> Ltagged ()
  (Pattern : Pattern (p) -> Pattern ()
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

(define (primfun? x)
  (list? (member x '(stop spawn send become from msg parent state
                          self set-state! yield))))

(define (cont? x)
  (and
    [symbol? x]
    [>= (string-length (symbol->string x)) 6]
    [equal? (substring (symbol->string x) 0 6) "_cont-"]
    ))

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
    (- (let (ma* ...) e))
    (- (call e0 e1 ma* ...))))

(define-pass desugar : Ltagged (l) -> Ldesugared ()
  (Pattern : Pattern (p) -> Pattern ())
  (split-arm : MatchArm (ma) -> * ()
    [(,p ,e) (cons p e)])
  (MatchArm : MatchArm (ma) -> MatchArm ())
  (Expr : Expr (e) -> Expr ()
    [(,f) `(,f)]
    [(,f ,e* ...)
     (case f
       ['become
        (let ([b (car e*)] [s (cadr e*)])
          `(seq (set-state! ,(Expr s)) (become ,(Expr b))))]
       ['stay `(seq (set-state! ,(Expr (car e*))) (yield))]
       ['output `(send (value (sys Output)) ,(map Expr e*) ...)]
       ['reply `(seq (send (from) ,(map Expr e*) ...) (yield))]
       [else `(,f ,(map Expr e*) ...)])]
    [(let (,ma* ...) ,e)
     (let go ([mas ma*])
       (cond
         [(null? mas) (Expr e)]
         [else
           (let ([arm (split-arm (car mas))])
           `(match ,(Expr (cdr arm)) (,(Pattern (car arm)) ,(go (cdr mas)))))]))]
    [(call ,e0 ,e1 ,ma* ...)
     `(seq (send ,(Expr e0) ,(Expr e1)) (recv ,(map MatchArm ma*) ...))]
    ))

(define-language
  Lconts
  (extends Ldesugared)
  (terminals
    (+ (cont (k))))
  (Expr (e)
    (- (recv ma* ...))
    (+ (with/cc (k) e))
    (+ (continue k e))))

(define-pass continuation-constructs : Ldesugared (l) -> Lconts ()
  (definitions
    (define cont-counter 0)
    (define (fresh-cont)
      (let* ([c cont-counter]
             [k (string->symbol (format "_cont-~s" c))])
        (set! cont-counter (+ c 1)) k)))
  (MatchArm : MatchArm (ma) -> MatchArm ())
  (Expr : Expr (e) -> Expr ()
    [(recv ,ma* ...)
     (let ([k (fresh-cont)])
       `(with/cc (,k)
          (become
            (actor [_ (continue ,k (match (msg) ,(map MatchArm ma*) ...))])
            )))]))

(define primfun-to-monadfun
  '((stop 0 stopM)
    (spawn 2 spawnM)
    (send 2 sendM)
    (become 1 becomeM)
    (from 0 fromM)
    (msg 0 msgM)
    (parent 0 parentM)
    (state 0 stateM)
    (self 0 selfM)
    (set-state! 1 set-stateM)
    (yield 0 yieldM)
    ))

(define (monadfun? x)
  (list? (member x (map caddr primfun-to-monadfun))))

(define-language
  Lmonad
  (extends Lconts)
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
    (+ (>>= e0 (av) e1))
    (- (pf))
    (+ mf)
    (- (pf e* ...))
    (+ (mf v* ...))
    (- (seq e* ...))
    (- (cons e0 e1))
    (- (match e ma* ...))
    (+ (match v ma* ...))
    (- (continue k e))
    (+ (continue k v))))

(define-pass to-monad : Lconts (l) -> Lmonad ()
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
         [else `(>>= ,(Expr (car es))
                     (,(fresh-anf-var))
                     ,(go (cdr es)))]))]
    [(actor ,ma* ...) `(close ,(mk-actor ma*))]
    [(match ,e ,ma* ...)
     (let ([a (fresh-anf-var)])
       `(>>= ,(Expr e) (,a) (match ,(mk-anf-val a) ,(map MatchArm ma*) ...)))]
    [(continue ,k ,e)
     (let ([a (fresh-anf-var)])
       `(>>= ,(Expr e) (,a) (continue ,k ,(mk-anf-val a))))]
    ))

(define-pass output-scheme : Lmonad (l) -> * ()
  (Pattern : Pattern (p) -> * ()
    [(atom ,a) `'(atom . ,a)]
    [(number ,n) `'(number . ,n)]
    [(bind ,bv) `'(bind . ,bv)]
    [(var ,bv) `'(var . ,bv)]
    [(sys ,s) `'(sys . ,s)]
    [,wp ''wildcard]
    [,null ''()]
    [(cons ,p0 ,p1) `(list 'cons ,(Pattern p0) ,(Pattern p1))])
  (Value : Value (v) -> * ()
    [(atom ,a) `'(atom . ,a)]
    [(sys ,s) `',s]
    [(anf-val ,av) av]
    [(number ,n) `'(number . ,n)]
    [,null ''()]
    [(cons ,v0 ,v1) `(list 'cons ,(Value v0) ,(Value v1))])
  (Expr : Expr (e) -> * ()
    [(point ,v) (list 'point (Value v))]
    [(match ,v ,ma* ...)
     (list 'matchM (Value v) (cons 'list (map MatchArm ma*)))]
    [(,mf ,v* ...) (cons mf (map Value v*))]
    [,mf mf]
    [(>>= ,e0 (,av) ,e1) `(>>= ,(Expr e0) (lambda (,av) ,(Expr e1)))]
    [(var ,bv) `(lookupM ',bv)]
    [(close ,e) `(closeM ,(Expr e))]
    [(continue ,k ,v) `(continueM ',k ,(Value v))]
    [(with/cc (,k) ,e) `(with/ccM ',k ,(Expr e))])
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

(define-language
  Lstack
  (extends Lmonad)
  (terminals
    (- (anf-val (av)))
    (- (bound-var (bv)))
    (- (cont (k))))
  (Pattern (p)
    (- (var bv)) (+ (slot n))
    (- (bind bv)) (+ (bind)))
  (Value (v)
    (- (anf-val av))
    (+ (slot n)))
  (ActorDef (ad)
    (- (define (bv) e))
    (+ (define e)))
  (Expr (e)
    (- (with/cc (k) e)) (+ (with/cc e))
    (- (var bv))
    (- (continue k v)) (+ (continue n v))
    (- (>>= e0 (av) e1)) (+ (>>= e0 e1)))
  (Options (o)
    (- (init bv v)) (+ (init n v))))

(define (index a b)
  (let [(tail (member a (reverse b)))]
    (and tail (length (cdr tail)))))

(define-pass to-stack : Lmonad (l) -> Lstack ()
  (Value : Value (v ctx) -> Value ()
    [(anf-val ,av) `(slot ,(index av ctx))])
  (Pattern : Pattern (p ctx) -> Pattern (ctx)
    [(var ,bv) (values `(slot ,(index bv ctx)) ctx)]
    [(bind ,bv) (values `(bind) (cons bv ctx))]
    [(cons ,p0 ,p1)
     (let*-values (([p0^ ctx^] (Pattern p0 ctx))
                   ([p1^ ctx^^] (Pattern p1 ctx^)))
       (values `(cons ,p0^ ,p1^) ctx^^))])
  (MatchArm : MatchArm (ma ctx) -> MatchArm ()
    [(,p ,e) (let-values ([(p^ ctx^) (Pattern p ctx)])
               `(,p^ ,(Expr e ctx^)))])
  (Expr : Expr (e ctx) -> Expr ()
    [(var ,bv) `(point (slot ,(index bv ctx)))]
    [(match ,v ,ma* ...)
     `(match ,(Value v ctx)
             ,(map (lambda (ma) (MatchArm ma ctx)) ma*) ...)]
    [(with/cc (,k) ,e) `(with/cc ,(Expr e (cons k ctx)))]
    [(continue ,k ,v) `(continue ,(index k ctx) ,(Value v ctx))]
    [(>>= ,e0 (,av) ,e1) `(>>= ,(Expr e0 ctx) ,(Expr e1 (cons av ctx)))])
  (ActorDef : ActorDef (ad ctx) -> ActorDef (ctx)
    [(define (,bv) ,e) (values `(define ,(Expr e ctx)) (cons bv ctx))])
  (Options : Options (o ctx) -> Options ()
    [(init ,bv ,v) `(init ,(index bv ctx) ,(Value v ctx))])
  (System : System (s) -> System ()
    [(system (,o* ...) ,ad* ...)
     (let* ([acc (fold-left
                   (lambda (acc ad)
                     (let-values ([(ad^ ctx^) (ActorDef ad (car acc))])
                       (cons ctx^ (cons ad^ (cdr acc)))))
                   (cons '() '()) ad*)]
            [ad*^ (reverse (cdr acc))]
            [ctx^ (car acc)]
            [o*^ (map (lambda (o) (Options o ctx^)) o*)])
       `(system (,o*^ ...) ,ad*^ ...))]))

(define (intercalate a xs)
  (cond
    [(null? xs) '()]
    [(null? (cdr xs)) xs]
    [else (cons (car xs) (cons a (intercalate a (cdr xs))))]))

(define-pass output-c : Lstack (l) -> * ()
  (definitions
    (define atoms (make-eq-hashtable))
    (define (internalize-atom a)
      (or (hashtable-ref atoms a #f)
          (begin
            (let ([hash (symbol-hash a)])
              (hashtable-set! atoms a hash)
              hash)))))
  (Value : Value (v) -> * ()
    [(slot ,n) (format "stack_nth(st,~D)" n)]
    [(sys ,s) (format "{.t=SYS,.v=~A}" (string-upcase (symbol->string s)))]
    [(number ,n) (format "{.t=NUMBER,.v=~D}" n)]
    [(atom ,a) (format "{.t=ATOM,.v=~D}" (internalize-atom a))]
    [(cons ,v0 ,v1)
     (format "{.t=CONS,.v=mk_cons(~A,~A)}" (Value v0) (Value v1))]
    [,null "{.t=NULL,.v=0}"])
  (Expr : Expr (e) -> * ()
    [(>>= ,e0 ,e1) (format "(stack_push(st,~A),~A)" (Expr e0) (Expr e1))]
    [(point ,v) (Value v)]
    [(match ,v ,ma* ...) (format "match(~A)" (Value v))]
    [,mf (format "~A()" mf)]
    [(,mf ,v* ...) (format "~A(~A)" mf
                           (fold-left string-append ""
                                      (intercalate "," (map Value v*))))]
    [(continue ,n ,v) (format "continue(stack_nth(st,~D),~A)" n (Value v))]
    [(with/cc ,e) (format "with_cc(~A)" (Expr e))]
    [(close ,e) (format "close(~A)" (Expr e))]
    )
  (ActorDef : ActorDef (ad) -> * ()
    [(define ,e) (format "stack_push(st,close(~A))" (Expr e))])
  (System : System (s) -> * ()
    [(system (,o* ...) ,ad* ...)
     (fold-left string-append "" (intercalate ";" (map ActorDef ad*)))])
  )
