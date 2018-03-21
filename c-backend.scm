(import (utils) (chezscheme) (posix))
(define-record-type c-env (fields cc flags includedir libdir output))

(define (gcc-with-output o)
  (make-c-env "gcc" (list "-g") "runtime/include" "runtime" o))

(define gcc-a-out (gcc-with-output "a.out"))

(define utf-8-transcoder
  (make-transcoder (utf-8-codec)))

(define (c-backend c-code opts)
  (let ([cmd
          (format "~a -o ~a ~a -I~a -L~a -x c -include preamble.c - -lruntime"
                  (c-env-cc opts)
                  (c-env-output opts)
                  (mk-string " " (c-env-flags opts))
                  (c-env-includedir opts)
                  (c-env-libdir opts)
                  )])
    (log-debug "execute: ~a" cmd)
    (log-trace-lines "c-code" (line-numbers c-code))
    (apply
      (lambda (from-stdout to-stdin pid)
        (put-string to-stdin c-code)
        (flush-output-port to-stdin)
        (close-output-port to-stdin)
        (let ([o (let go ([acc ""])
                   (let ([l (get-line from-stdout)])
                     (cond
                       [(eof-object? l) acc]
                       [else (go (string-append acc l "\n" ))])))]
              [ec (wait-for-pid pid)])
          (log-debug "c compiler exited with code: ~D" ec)
          (log-debug "c compiler output: <<<~n~a>>>" o)
          (eq? ec 0)
          ))
      (process (string-append "exec " cmd " 2>&1"))))
  )
