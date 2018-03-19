(library
  (utils)
  (export intercalate mk-string log-info log-debug)
  (import (scheme))

  (define (intercalate a xs)
    (cond
      [(null? xs) '()]
      [(null? (cdr xs)) xs]
      [else (cons (car xs) (cons a (intercalate a (cdr xs))))]))

  (define (mk-string sep ss) (fold-left string-append "" (intercalate sep ss)))

  (define metadata-port (console-error-port))

  (define (log-info fmt . args)
    (display-string
      (eval `(format (string-append "INFO: " ,fmt "~n") . ,args))
      metadata-port))

  (define (log-debug fmt . args)
    (display-string
      (eval `(format (string-append "DEBUG: " ,fmt "~n") . ,args))
      metadata-port))
  )
