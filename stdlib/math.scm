(define (abs x) ((if (< x 0) - +)  x))
(define (almost-equal x y delta) 
    (> delta (abs (- x y))))

(define (sqrt-prime x last-x)
    (if (almost-equal (/ (+ x last-x) 2) x 0.000001) x
        (sqrt-prime (/ (+ x last-x) 2) x)))

(define (sqrt x) (sqrt-prime x 1))

(define (quad x) (* x x))

(define (pow x n) 
    (if (<= n 0) 1
    (if (= n 1) x 
        (* (pow x (- n 1)) x))))
