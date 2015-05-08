(define (and . lst) "logical and on multiple values" (fold && #t lst))
(define (or . lst) "logical or on multiple values" (fold || #f lst))

(define (not x) "logical not" (if x #f #t))
