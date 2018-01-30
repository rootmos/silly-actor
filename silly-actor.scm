(import (matchable) (nanopass))

(define verbs '(match stop spawn send become))

(define (atom? x) (and (symbol? x) (not (member x verbs))))
(define (verb? x) (list? (member x '(match stop spawn send become))))
(define (variable-pattern? x) (match x [('unquote s) (atom? s)] [else #f]))

(define-language
  Lsrc
  (terminals
    (atom (a))
    (verb (v))
    (number (n))
    (variable-pattern (vp)))
  (entry System)
  (Pattern (p)
    vp
    a
    n
    (p* ...))
  (ActorDef (ad)
    (actor a p))
  (System (t)
    (system a ad* ...)))

(define-parser parse-Lsrc Lsrc)
