(import (utils) (chezscheme) (posix))
(define-record-type c-env (fields cc flags includedir libdir output))

(define (gcc-with-output o)
  (make-c-env "gcc" (list "-g")
              (list "runtime/include" "bdwgc-dist/include")
              (list "runtime" "bdwgc-dist/lib") o))

(define gcc-a-out (gcc-with-output "a.out"))

(define utf-8-transcoder
  (make-transcoder (utf-8-codec)))

(define (c-backend c-code opts)
  (let ([cmd
          (format "~a -static -o ~a ~a -lruntime -lgc -include preamble.c -x c -"
                  (c-env-cc opts)
                  (c-env-output opts)
                  (mk-string " "
                    (append
                      (c-env-flags opts)
                            (map (lambda (d) (format "-I~a" d))
                                 (c-env-includedir opts))
                            (map (lambda (d) (format "-L~a" d))
                                 (c-env-libdir opts)))))])
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
