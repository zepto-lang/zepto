; This is purely homegrown math. It's best you don't use it.
; Not even once.
(define (precision x) "convert to float; x: value" (* x 1.0))
(define (abs x) "absolute value of; x: value" ((if (< x 0) - +)  x))
(define (almost-equal x y delta)  "internal function for sqrt"
    (> delta (abs (- x y))))

(define (square x) "square a value; x: value to be squared" (* x x))

(define (pow x n) "power of; x: base, n: exponent"
    (if (<= n 1)
      1
      (* n (pow x (- n 1)))))

(define expt pow)
(define modulo mod)
