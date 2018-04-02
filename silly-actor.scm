(import (matchable) (nanopass) (utils))

(define primfun-to-monadfun
  '((stop 0 stopM)
    (spawn 2 spawnM)
    (send 2 sendM)
    (from 0 fromM)
    (msg 0 msgM)
    (parent 0 parentM)
    (state 0 stateM)
    (self 0 selfM)
    (set-state! 1 set-stateM)
    (set-cl! 1 set-clM)
    (yield 0 yieldM)
    (= 2 equalM)
    (+ 2 addM)
    ))

(define syntactic-sugar '(become stay output reply))

(define (fun? x)
  (list? (member x (append (map car primfun-to-monadfun) syntactic-sugar))))

(define (atom? x)
  (and
    [symbol? x]
    [not (fun? x)]
    [char-lower-case? (car (string->list (symbol->string x)))]
    [not (equal? (substring (symbol->string x) 0 1) "_")]
    ))

(define (sys? x)
  (list? (member x '(Init Match_error Stopped Output Died))))

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
    (bound-var (bv)))
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
    (init bv v))
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

(define (primfun? x) (list? (member x (map car primfun-to-monadfun))))

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
          `(seq (set-state! ,(Expr s)) (set-cl! ,(Expr b)) (yield)))]
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
    (+ (continue k e))
    ))

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
          (seq
            (set-cl! (actor
                       [_ (continue ,k
                            (match (msg) ,(map MatchArm ma*) ...))]))
            (yield))))]))

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

(define-pass inline-values : Lmonad (l) -> Lmonad ()
  (Value : Value (v ctx) -> Value ()
    [(anf-val ,av) (cond [(assv av ctx) => cdr] [else v])])
  (MatchArm : MatchArm (ma ctx) -> MatchArm ()
    [(,p ,e) `(,p ,(Expr e ctx))])
  (Expr : Expr (e ctx) -> Expr ()
    [(>>= (point ,v) (,av) ,e1) (Expr e1 (cons (cons av v) ctx))])
  (ActorDef : ActorDef (ad) -> ActorDef ()
    [(define (,bv) ,e) `(define (,bv) ,(Expr e '()))])
  (System : System (s) -> System ()
    [(system (,o* ...) ,ad* ...)
     `(system (,o* ...) ,(map ActorDef ad*) ...)]))

(define-pass match-conts : Lmonad (l) -> Lmonad ()
  (definitions
    (define anf-m-counter 0)
    (define (fresh-anf-m-var)
      (let* ([c anf-m-counter]
             [a (string->symbol (format "_anf-m-~s" c))])
        (set! anf-m-counter (+ c 1)) a))

    (define mcont-counter 0)
    (define (fresh-mcont)
      (let* ([c mcont-counter]
             [k (string->symbol (format "_cont-m-~s" c))])
        (set! mcont-counter (+ c 1)) k)))
  (MatchArm : MatchArm (ma k) -> MatchArm ()
    [(,p ,e)
     (let ([a (fresh-anf-m-var)])
     `(,p (>>= ,(Expr e) (,a) (continue ,k (anf-val ,a)))))])
  (Expr : Expr (e) -> Expr ()
    [(match ,v ,ma* ...)
     (let ([k (fresh-mcont)])
       `(with/cc (,k)
          (match ,v ,(map (lambda (ma) (MatchArm ma k)) ma*) ...)))]))

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
    [(number ,n) `(cons 'number ,n)]
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
    [(init ,bv ,v) `(list 'init ',bv ,(Value v))])
  (System : System (s) -> * ()
    [(system (,o* ...) ,ad* ...)
     `(run-system
        ,(cons 'list (map Options o*))
        ,(cons 'list  (map ActorDef ad*)))]))

(define-language
  Lde-bruijn
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

(define-pass de-bruijn : Lmonad (l) -> Lde-bruijn ()
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
    [(,p ,e)
     (let*-values ([(p^ ctx^) (Pattern p ctx)]
                   [(e^ ctx^^) (Expr e ctx^)])
               `(,p^ ,e^))])
  (Expr : Expr (e ctx) -> Expr (ctx)
    [(var ,bv)
     (values `(point (slot ,(index bv ctx))) ctx)]
    [(match ,v ,ma* ...)
     (values
       `(match ,(Value v ctx)
               ,(map (lambda (ma) (MatchArm ma ctx)) ma*) ...)
       ctx)]
    [(with/cc (,k) ,e)
     (let-values ([(e^ ctx^) (Expr e (cons k ctx))])
       (values `(with/cc ,e^) ctx))]
    [(close ,e)
     (let-values ([(e^ ctx^) (Expr e ctx)])
       (values `(close ,e^) ctx))]
    [(continue ,k ,v)
     (values `(continue ,(index k ctx) ,(Value v ctx)) ctx)]
    [(>>= ,e0 (,av) ,e1)
     (let*-values ([(e0^ ctx^) (Expr e0 ctx)]
                   [(e1^ ctx^^) (Expr e1 (cons av ctx^))])
       (values `(>>= ,e0^ ,e1^) ctx^^))])
  (ActorDef : ActorDef (ad ctx) -> ActorDef (ctx)
    [(define (,bv) ,e)
     (let-values ([(e^ ctx^) (Expr e ctx)])
       (values `(define ,e^) (cons bv ctx)))])
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

(define (closure? x)
  (and
    [symbol? x]
    [>= (string-length (symbol->string x)) 3]
    [equal? (substring (symbol->string x) 0 3) "cl_"]
    ))

(define monadfun-to-runtimefun
  '((stopM stopR)
    (spawnM spawnR)
    (sendM sendR)
    (fromM fromR)
    (msgM msgR)
    (parentM parentR)
    (stateM stateR)
    (selfM selfR)
    (set-stateM set_stateR)
    (set-clM set_clR)
    (equalM equalR)
    (addM addR)
    ))

(define (runtimefun? x) (list? (member x (map cadr monadfun-to-runtimefun))))

(define-language
  Lstack
  (terminals
    (number (n))
    (runtimefun (rf))
    (sys (s))
    (atom (a))
    (closure (cl)))
  (entry System)
  (Value (v)
    (nil)
    (number n)
    (nth n)
    (car v)
    (cdr v)
    (cons v0 v1)
    (arg)
    (sys s)
    (atom a))
  (Expr (e)
    v
    rf
    (rf v* ...)
    (closure-ref cl))
  (Cond (c)
    (true)
    (eq v0 v1)
    (and c0 c1)
    (is-cons v))
  (CondArm (ca) (c st))
  (Statement (st)
    (push e)
    (discard e)
    (yield)
    (match-error v)
    (continue v0 v1)
    (cond ca* ...)
    (and_then st0 st1))
  (Closure (closure)
    (nullary cl st)
    (unary cl st))
  (System (syst)
    (system (closure* ...) st)))

(define-pass to-stack : Lde-bruijn (l) -> Lstack ()
  (definitions
    (define cl-counter 0)
    (define cls '())
    (with-output-language (Lstack Value)
      (define mk-nil `(nil))
      (define mk-arg `(arg))
      (define (mk-nth n) `(nth ,n))
      (define (car^ v) `(car ,v))
      (define (cdr^ v) `(cdr ,v))
      )
    (with-output-language (Lstack Expr)
      (define (mk-cl cl) `(closure-ref ,cl)))
    (with-output-language (Lstack CondArm)
      (define (match-error-arm v)
        `((true) (match-error ,v))))
    (with-output-language (Lstack Closure)
      (define (close k arity)
        (let* ([c cl-counter] [cl (string->symbol (format "cl_~s" c))])
          (set! cl-counter (+ c 1))
          (set! cls (cons (case arity
                            [0 `(nullary ,cl ,(k mk-nil))]
                            [1 `(unary ,cl ,(k mk-arg))])
                          cls))
          (mk-cl cl)))))
  (Value : Value (r) -> Value ()
    [(slot ,n) `(nth ,n)]
    [,null `(nil)])
  (Pattern : Pattern (p v) -> Cond (bs)
    [(cons ,p0 ,p1)
     (let-values
       ([(cs0 bs0) (Pattern p0 (car^ v))]
        [(cs1 bs1) (Pattern p1 (cdr^ v))])
       (values `(and (is-cons ,v) (and ,cs0 ,cs1)) (append bs1 bs0)))]
    [(bind) (values `(true) (list v))]
    [(slot ,n) (values `(eq ,v (nth ,n)) '())]
    [(sys ,s) (values `(eq ,v (sys ,s)) '())]
    [(atom ,a) (values `(eq ,v (atom ,a)) '())]
    [(number ,n) (values `(eq ,v (number ,n)) '())]
    [,null (values `(eq ,v (nil)) '())]
    [,wp (values `(true) '())])
  (MatchArm : MatchArm (ma v) -> CondArm ()
    [(,p ,e)
     (let-values ([(cs bs) (Pattern p v)])
      `(,cs
         ,(let go ([bs^ bs])
             (case bs^
               ['() (Expr e (lambda (w) w))]
               [else `(and_then (push ,(car bs^)) ,(go (cdr bs^)))]))))])
  (Expr : Expr (e k) -> Statement ()
    [(point ,v) (k (Value v))]
    [(match ,v ,ma* ...)
     (let ([arms (append (map (lambda (ma) (MatchArm ma (Value v))) ma*)
                         (list (match-error-arm (Value v))))])
       `(cond ,arms ...))]
    [,mf
      (cond
        [(eq? mf 'yieldM) `(yield)]
        [(assv mf monadfun-to-runtimefun)
         => (lambda (l)
              (k (with-output-language (Lstack Expr) `,(cadr l))))]
        [else (error 'output-c "malformed monadfun to runtimefun conversion"
                     (unparse-Lde-bruijn e))])]
    [(,mf ,v* ...)
      (cond
        [(assv mf monadfun-to-runtimefun)
         => (lambda (l)
              (k (with-output-language (Lstack Expr)
                   `(,(cadr l) ,(map Value v*) ...))))]
        [else (error 'output-c "malformed monadfun to runtimefun conversion"
                     (unparse-Lde-bruijn e))])]
    [(continue ,n ,v) `(continue (nth ,n) ,(Value v))]
    [(close ,e)
     (k (close (lambda (_) (Expr e (lambda (v) `(discard ,v)))) 0))]
    [(>>= ,e0 ,e1) (Expr e0 (lambda (z) `(and_then (push ,z) ,(Expr e1 k))))]
    [(with/cc ,e)
     `(and_then (push ,(close k 1)) ,(Expr e (lambda (w) `(discard ,w))))])
  (ActorDef : ActorDef (ad) -> Statement ()
    [(define ,e)
     `(push ,(close
               (lambda (_)
                 (Expr e (lambda (w)
                           (with-output-language (Lstack Statement)
                                                      `(discard ,w))))) 0))])
  (Options : Options (o) -> Statement ()
    [(init ,n ,v) `(discard (spawnR ,(list (mk-nth n) (Value v)) ...))])
  (System : System (syst) -> System ()
    [(system (,o* ...) ,ad* ...)
     (let ([as (let go ([sts (append (map ActorDef ad*) (map Options o*))])
                 (cond
                   [(null? (cdr sts)) (car sts)]
                   [else (with-output-language (Lstack Statement)
                           `(and_then ,(car sts) ,(go (cdr sts))))]))])
       `(system (,(reverse cls) ...) ,as))]))

(define-language
  Lstack-with-atom-defs
  (extends Lstack)
  (terminals
    (+ (string (str)))
    (- (atom (a))))
  (Value (v)
    (- (atom a))
    (+ (atom n)))
  (AtomDef (ad)
    (+ (n str)))
  (System (syst)
    (- (system (closure* ...) st))
    (+ (system (ad* ...) (closure* ...) st))))

(define predefined-atoms '(true false))
(define (hash-atom a) (symbol-hash a))

(define-pass collect-atoms : Lstack (l) -> Lstack-with-atom-defs ()
  (definitions
    (define atoms (make-eq-hashtable))
    (define (internalize-atom a)
      (or (hashtable-ref atoms a #f)
          (begin
            (let ([hash (hash-atom a)])
              (hashtable-set! atoms a hash) hash))))
    (with-output-language (Lstack-with-atom-defs AtomDef)
      (define (mk-atom-defs)
        (for-each internalize-atom predefined-atoms)
        (let-values ([(keys vals) (hashtable-entries atoms)])
          (let go ([ks (vector->list keys)] [vs (vector->list vals)] [acc '()])
            (cond
              [(pair? ks)
               (go (cdr ks) (cdr vs)
                   (cons `(,(car vs) ,(symbol->string (car ks))) acc))]
              [else acc]))))))
  (Value : Value (v) -> Value ()
    [(atom ,a) `(atom ,(internalize-atom a))])
  (Statement : Statement (st) -> Statement ())
  (Closure : Closure (cl) -> Closure ())
  (System : System (syst) -> System ()
    [(system (,closure* ...) ,st)
     (let ([cl* (map Closure closure*)] [st^ (Statement st)])
       `(system (,(mk-atom-defs) ...) (,cl* ...) ,st^))]))

(define-pass output-c : Lstack-with-atom-defs (l) -> * ()
  (definitions
    (define unusable-arg
      (lambda () (error 'output-c "used argument in nullary closure")))
    (define var-counter 0)
    (define (fresh-var)
      (let* ([c var-counter] [v (format "var_~s" c)])
        (set! var-counter (+ c 1)) v)))
  (Value : Value (v arg) -> * ()
    [(arg) (arg)]
    [(atom ,n) (format "mk_atom(~d)" n)]
    [(number ,n) (format "mk_number(~d)" n)]
    [(nth ,n) (format "nth(~d)" n)]
    [(car ,v) (format "car(~d)" (Value v arg))]
    [(cdr ,v) (format "cdr(~d)" (Value v arg))]
    [(cons ,v0 ,v1)
     (format "mk_cons(~a,~a)" (Value v0 arg) (Value v1 arg))]
    [(nil) "mk_nil()"]
    [(sys ,s) (format "mk_sys(~A)" (string-upcase (symbol->string s)))])
  (Expr : Expr (e arg) -> * ()
    [,v (Value v arg)]
    [,rf (format "~A()" rf)]
    [(,rf ,v* ...)
     (format "~A(~A)" rf (mk-string "," (map (lambda (v) (Value v arg)) v*)))]
    [(closure-ref ,cl) (format "mk_cl(~a)" cl)])
  (Cond : Cond (c arg) -> * ()
    [(true) "true"]
    [(eq ,v0 ,v1) (format "eq(~a,~a)" (Value v0 arg) (Value v1 arg))]
    [(and ,c0 ,c1) (format "~a && ~a" (Cond c0 arg) (Cond c1 arg))]
    [(is-cons ,v) (format "is_cons(~a)" (Value v arg))])
  (CondArm : CondArm (ca arg) -> * ()
    [(,c ,st) (values (Cond c arg) (Statement st arg))])
  (Statement : Statement (st arg) -> * ()
    [(push ,e) (format "push(~a)" (Expr e arg))]
    [(discard ,e) (Expr e arg)]
    [(yield) "return yield()"]
    [(match-error ,v) (format "return match_error(~A)" (Value v arg))]
    [(continue ,v0 ,v1)
     (format "return cont(~A,~A)" (Value v0 arg) (Value v1 arg))]
    [(cond ,ca* ...)
     (let go ([cas ca*])
       (let-values ([(c st) (CondArm (car cas) arg)])
         (cond
           [(null? (cdr cas)) (format "if(~a){~a;}" c st)]
           [else (format "if(~a){~a;}else ~a" c st (go (cdr cas)))])))]
    [(and_then ,st0 ,st1)
     (format "~a;~a" (Statement st0 arg) (Statement st1 arg))])
  (Closure : Closure (c) -> * ()
    [(unary ,cl ,st)
     (let ([arg (fresh-var)])
       (format "define_unary_closure(~A,~A)~A;end_closure();"
               cl arg (Statement st (lambda () arg))))]
    [(nullary ,cl ,st)
     (let ([arg (fresh-var)])
       (format "define_nullary_closure(~A)~A;end_closure();"
               cl (Statement st unusable-arg)))])
  (AtomDef : AtomDef (ad) -> * ()
    [(,n ,str) (format "atoms_entry(~a,\"~a\")" n str)])
  (System : System (s) -> * ()
    [(system (,ad* ...) (,closure* ...) ,st)
     (let ([s (list (format "define_system() ~a; end_system();"
                            (Statement st unusable-arg)))]
           [p (map (lambda (a) (format "predefined_atom(~a,~a);"
                                             (string-upcase (symbol->string a))
                                             (hash-atom a)))
                     predefined-atoms)]
           [as (list (format "atoms_begin() ~a atoms_end()"
                             (fold-left string-append "" (map AtomDef ad*))))]
           [cls (map Closure closure*)]
           )
       (fold-left string-append "" (append p as cls s)))]))
