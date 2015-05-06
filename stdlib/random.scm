;; This code is taken from:
;; http://stackoverflow.com/questions/14674165/scheme-generate-random
;; It is not to be used in cryptography or related fields.
(define random
  (let ((a 69069) (c 1) (m (expt 2 32)) (seed 19380110.0))
    (lambda (new-seed)
      (if (pair? new-seed)
        (begin (set! seed (car new-seed)))
        (begin (set! seed (modulo (+ (* seed a) c) m))))
      (/ seed m))))

(define (randint . args) "generate a random integer between the given args(the lower range is optional)"
  (cond ((= (length args) 1) (randint 0 (car args)))
        ((= (length args) 2)
         (+ (car args) (floor (* (random) (- (cadr args) (car args))))))
        (else (write "usage: (randint [lo] hi)"))))
