(define (big n)
    (if (<= n 0)
        0
        `(+ 1 1 1 1 1 1 1 1 1 1 ,(big (- n 1)))))

(define nst (big 1000000))

(write (eval nst))
(newline)
