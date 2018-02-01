(import (matchable) (nanopass))

(define verbs
  '(match stop spawn send become actor stay self from parent list))

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
  (Value (v)
    a
    n
    (actor (p) m* ...)
    '()
    (v* ...))
  (Expr (e)
    v
    (become e0 e1)
    (stay e)
    (spawn e0 e1)
    (send e0 e1)
    (stop)
    (self)
    (list e* ...))
  (MsgCase (m) (p e))
  (ActorDef (ad) (define (a p) m* ...))
  (Options (o) (init a v))
  (System (t) (system (o* ...) ad* ...)))

(define-parser parse-Lsrc Lsrc)

(define-pass output-scheme : Lsrc (e) -> * ()
  (Value : Value (v) -> * ()
    [,a 'a]
    [,n n]
    ['() '()]
    [(actor (,p) ,m* ...) (void)]
    [(,v* ...) (map Value v*)])
  (Pattern : Pattern (p) -> * ()
    [,a `'(atom ,a)]
    [,n `(number ,n)] 
    ['() '(nil)]
    [,vp `(bind ,(cadr vp))]
    [,wp ''wildcard]
    [(,p* ...) (map Pattern p*)])
  (Expr : Expr (e) -> * ()
    [,v (Value v)]
    [(become ,e0 ,e1) (list 'become (Expr e0) (Expr e1))]
    [(stay ,e) (list 'stay (Expr e))]
    [(spawn ,e0 ,e1) (list 'spawn (Expr e0) (Expr e1))]
    [(send ,e0 ,e1) (list 'send (Expr e0) (Expr e1))]
    [(stop) '(stop)]
    [(self) '(self)]
    [(list ,e* ...) (cons 'list (map Expr e*))])
  (ActorDef : ActorDef (ad) -> * ()
    [(define (,a ,p) ,m* ...)
     `(cons ',a (lambda (self from msg)
              (pattern-match (actor-state self) ,(Pattern p))
              (printf "self:~s from:~s msg:~s\n" (actor-id self) from msg)
              ))])
  (Options : Options (o) -> * ()
    [(init ,a ,v) `(init ,a ,(Value v))])
  (System : System (s) -> * ()
    [(system (,o* ...) ,ad* ...)
     `(run-system
        ',(map Options o*)
        ,(cons 'list  (map ActorDef (reverse ad*))))]))
