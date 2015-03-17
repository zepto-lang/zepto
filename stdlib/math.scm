(define exact? integer?)
(define (inexact? x) (and (real? x) (not (integer? x))))
(define (even? n) (= (remainder n 2) 0))
(define (odd? n) (not (= (remainder n 2) 0)))
(define (zero? n) (= n 0))
(define (positive? n) (> n 0))
(define (negative? n) (< n 0))
(define complex? number?)
(define (abs n) (if (>= n 0) n (- n)))
(define (exact->inexact n) (* n 1.0))
(define (<> n1 n2) (not (= n1 n2)))

(define (succ x) (+ x 1))

(define (pred x) (- x 1))

(define (gcd a b)
  (let ((aa (abs a))
    (bb (abs b)))
     (if (= bb 0)
          aa
          (gcd bb (remainder aa bb)))))

(define (lcm a b)
     (if (or (= a 0) (= b 0))
          0
          (abs (* (quotient a (gcd a b)) b))))
