silly-actor
===========
[![Build image and run tests](https://github.com/rootmos/silly-actor/actions/workflows/build.yaml/badge.svg)](https://github.com/rootmos/silly-actor/actions/workflows/build.yaml)

Examples
--------
### Example: 01
```scheme
(system
  ((init Main ()))
  (define (Main) (Init (output (state)))))
```
#### Output
```scheme
(())
```

### Example: 02
```scheme
(system
  ((init Main ()))
  (define (Main) (_ (seq (output 1) (output 2)))))
```
#### Output
```scheme
((number . 1) (number . 2))
```

### Example: 03
```scheme
(system
  ((init Main 7))
  (define (Main) (_ (output (state)))))
```
#### Output
```scheme
((number . 7))
```

### Example: 04
```scheme
(system
  ((init Main ()))
  (define (Main) (8 (output success)) (_ (send (self) 8))))
```
#### Output
```scheme
((atom . success))
```

### Example: 05
```scheme
(system
  ((init Main ()))
  (define (Main)
    (_ (seq (send (self) 8)
            (become (actor (8 (output success))) (state))))))
```
#### Output
```scheme
((atom . success))
```

### Example: 06
```scheme
(system
  ((init Main ()))
  (define (Aux) (7 (output (state))) (_ ()))
  (define (Main)
    (_ (let ([Id (spawn Aux success)]) (send Id 7)))))
```
#### Output
```scheme
((atom . success))
```

### Example: 07
```scheme
(system
  ((init Main ()))
  (define (Aux) (7 (output (state))) (_ ()))
  (define (Main) (_ (send (spawn Aux success) 7))))
```
#### Output
```scheme
((atom . success))
```

### Example: 08
```scheme
(system
  ((init Main ()))
  (define (Main)
    (_ (let ([X 7]) (match 7 ['X (output success)])))))
```
#### Output
```scheme
((atom . success))
```

### Example: 09
```scheme
(system
  ((init Main ()))
  (define (Main) (_ (match '(1 2) [(X _) (output X)]))))
```
#### Output
```scheme
((number . 1))
```

### Example: 10
```scheme
(system
  ((init Main ()))
  (define (Aux) (7 (reply 8)) (_ ()))
  (define (Main)
    (_ (seq (send (spawn Aux ()) 7)
            (output (recv (8 success)))))))
```
#### Output
```scheme
((atom . success))
```

### Example: 11
```scheme
(system
  ((init Main ()))
  (define (Aux)
    (7 (seq (reply 8) (reply 13)))
    (9 (reply 10))
    (_ ()))
  (define (Main)
    (_ (let ([Id (spawn Aux ())])
         (seq (output (call Id 7 (8 a)))
              (output (call Id 9 (10 b))))))))
```
#### Output
```scheme
((atom . a) (atom . b))
```

### Example: 12
```scheme
(system
  ((init Main ()))
  (define (Aux) (Init (send (parent) foo)))
  (define (Main) (Init (spawn Aux ())) (foo (output bar))))
```
#### Output
```scheme
((atom . bar))
```

### Example: 13
```scheme
(system
  ((init Main ()))
  (define (Aux) (na (output failure)))
  (define (Main)
    (Init (stay (spawn Aux ())))
    ((Died Match_error Msg)
      (let ([true (= (state) (from))]) (output Msg)))))
```
#### Output
```scheme
((sys . Init))
```

### Example: 14
```scheme
(system
  ((init Main ()))
  (define (Counter)
    (poke (stay (+ (state) 1)))
    (tell (output (state)))
    (_ ()))
  (define (Main)
    (Init
      (let ([Id (spawn Counter 0)])
        (match (send Id poke)
          [1 (output whoops)]
          [2 (output whoops)]
          [(A B) (output whoops)]
          [() (send Id tell)])))))
```
#### Output
```scheme
((number . 1))
```

### Example: 15
```scheme
(system
  ((init Main ()))
  (define (Main)
    (Init (send (self) 0))
    (X (match (= X 100000)
         [false (send (self) (+ X 1))]
         [true (output success)]))))
```
#### Output
```scheme
((atom . success))
```

### Example: arith
```scheme
(system
  ((init Main ()))
  (define (Main) (_ (seq (output (+ 1 2)) (output (+ 0 7))))))
```
#### Output
```scheme
((number . 3) (number . 7))
```

### Example: eq
```scheme
(system
  ((init Main ()))
  (define (Main)
    (_ (seq (output (= 1 1)) (output (= 1 2)) (output (= 1 foo))
            (output (= foo foo)) (output (= foo bar)) (output (= () ()))
            (output (= 1 ()))))))
```
#### Output
```scheme
((atom . true) (atom . false) (atom . false) (atom . true)
  (atom . false) (atom . true) (atom . false))
```

