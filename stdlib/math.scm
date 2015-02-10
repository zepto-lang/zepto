; This is purely homegrown math. It's best you don't use it.
; Not even once.
(define (precision x) (* x 1.0))
(define (abs x) ((if (< x 0) - +)  x))
(define (almost-equal x y delta) 
    (> delta (abs (- x y))))

(define (sqrt-prime x last-x)
    (if (almost-equal (/ (+ x last-x) 2) x 0.000001) (/ x 2)
        (sqrt-prime (/ (+ x last-x) 2) x)))

; This routine is broken
(define (sqrt x) (sqrt-prime (precision x) 1))

(define (quad x) (* x x))

(define (pow x n) 
    (if (<= n 0) 1
    (if (= n 1) x 
        (* (pow x (- n 1)) x))))
