(define (and . lst) (fold && #t lst))
(define (or . lst) (fold || #f lst))

(define (not x) (if x #f #t))

(define (null? obj)
    (if (eqv? obj '())
      #t
      #f))
