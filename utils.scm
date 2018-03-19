(library
  (utils)
  (export intercalate mk-string line-numbers
          log-info log-debug log-debug-lines)
  (import (scheme) (thunder-utils))

  (define (intercalate a xs)
    (cond
      [(null? xs) '()]
      [(null? (cdr xs)) xs]
      [else (cons (car xs) (cons a (intercalate a (cdr xs))))]))

  (define (mk-string sep ss) (fold-left string-append "" (intercalate sep ss)))

  (define (line-numbers s)
    (let go ([ls (string-split s (list #\newline))] [n 1] [acc '()])
      (cond
        [(null? ls) (reverse acc)]
        [else (go (cdr ls) (+ n 1)
                  (cons (format "~D: ~a" n (car ls)) acc))])))

  (define metadata-port (console-error-port))

  (define (log-info fmt . args)
    (display-string
      (eval `(format (string-append "INFO: " ,fmt "~n") . ,args))
      metadata-port))

  (define (log-debug fmt . args)
    (display-string
      (eval `(format (string-append "DEBUG: " ,fmt "~n") . ,args))
      metadata-port))

  (define (log-debug-lines prefix ls)
    (for-each
      (lambda (l) (display-string (string-append "DEBUG: " prefix " " l "\n") metadata-port))
      ls))
  )
