silly-actor
===========
[![Build Status](https://travis-ci.org/rootmos/silly-actor.svg?branch=master)](https://travis-ci.org/rootmos/silly-actor)

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

