(library
  (utils)
  (export intercalate mk-string line-numbers
          index
          log-warn log-info log-debug log-trace-lines)
  (import (scheme) (thunder-utils))

  (define (index a b)
    (let [(tail (member a (reverse b)))]
      (and tail (length (cdr tail)))))

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

  (define log-level
    (cond
      [(equal? (getenv "TRACE") "1") 4]
      [(equal? (getenv "DEBUG") "1") 3]
      [(equal? (getenv "INFO") "1") 2]
      [(equal? (getenv "WARN") "1") 1]
      [else 0]))

  (define (log-raw tag l fmt args)
    (when (>= log-level l)
      (display-string
        (apply format (cons (string-append tag ": " fmt "~n") args))
        metadata-port)))

  (define (log-warn fmt . args) (log-raw "WARN" 1 fmt args))
  (define (log-info fmt . args) (log-raw "INFO" 2 fmt args))
  (define (log-debug fmt . args) (log-raw "DEBUG" 3 fmt args))
  (define (log-trace fmt . args) (log-raw "TRACE" 4 fmt args))

  (define (log-trace-lines prefix ls)
    (when (>= log-level 4)
      (for-each (lambda (l) (log-debug (string-append prefix " " l))) ls)))
  )
