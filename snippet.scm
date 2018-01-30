(load "silly-actor.scm")

(pretty-print
  [list
    (atom? 'foobar)
    (atom? 'match)
    (atom? 'match)
    (verb? 'match)
    (verb? 'foobar)
    (variable-pattern? '(test Foo))
    (variable-pattern? ',foo)
    (variable-pattern? ',match)
    ])

(pretty-print (parse-Lsrc '(system main (actor main s) (actor aux s))))
