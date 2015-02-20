; This is purely homegrown math. It's best you don't use it.
; Not even once.
(define (precision x) "convert to float; x: value" (* x 1.0))
(define (abs x) "absolute value of; x: value" ((if (< x 0) - +)  x))
(define (almost-equal x y delta)  "internal function for sqrt"
    (> delta (abs (- x y))))

(define (sqrt-prime x last-x) "internal function for sqrt"
    (let ((next-x (/ (+ x last-x) 2.0)))
        (if (almost-equal next-x x 0.0000001) x
            (sqrt-prime next-x x))))

; This routine is broken
(define (sqrt x) "square root of; x: base; routine is broken"
    (sqrt-prime (precision x) 1.0))

(define (square x) "square a value; x: value to be squared" (* x x))

(define (pow x n) "power of; x: base, n: exponent"
    (if (<= n 0) 1
    (if (= n 1) x 
        (* (pow x (- n 1)) x))))

(define expt pow)
(define modulo mod)
