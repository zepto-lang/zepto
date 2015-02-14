; This is purely homegrown math. It's best you don't use it.
; Not even once.
(define (precision x) (* x 1.0))
(define (abs x) ((if (< x 0) - +)  x))
(define (almost-equal x y delta) 
    (> delta (abs (- x y))))

(define (sqrt-prime x last-x)
    (let ((next-x (/ (+ x last-x) 2.0)))
        (if (almost-equal next-x x 0.0000001) x
            (sqrt-prime next-x x))))

; This routine is broken
(define (sqrt x) (sqrt-prime (precision x) 1.0))

(define (quad x) (* x x))

(define (pow x n) 
    (if (<= n 0) 1
    (if (= n 1) x 
        (* (pow x (- n 1)) x))))
