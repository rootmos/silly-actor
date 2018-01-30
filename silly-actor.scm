(import (matchable) (nanopass))

(define verbs '(match stop spawn send become actor stay self))

(define (verb? x) (list? (member x verbs)))
(define (atom? x)
  (and
    [symbol? x]
    [not (verb? x)]
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
  (MsgCase (m)
    (p e))
  (UnnamedActor (ua)
    (actor (a) m* ...))
  (ActorDef (ad)
    (define (a p) m* ...))
  (Options (o)
    (start a))
  (System (t)
    (system (o* ...) ad* ...)))

(define-parser parse-Lsrc Lsrc)
