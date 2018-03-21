(define (generate-readme tcs)
  (with-output-to-file "README.md" (lambda ()
    (printf "silly-actor~n")
    (printf "===========~n")

    (printf "~n")

    (printf "Examples~n")
    (printf "--------~n")

    (for-each (lambda (tc)
      (printf "## Example: ~a~n" (test-case-name tc))
      (printf "```scheme~n")
      (pretty-print (test-case-source tc))
      (printf "```~n")
      (printf "### Output~n")
      (printf "```scheme~n")
      (pretty-print (test-case-expected tc))
      (printf "```~n~n")
      )
      tcs)
    )
    'truncate
    ))
