(define (big n)
    (if (<= n 0)
        0
        `(+ 1 1 1 1 1 1 1 1 1 1 ,(big (- n 1)))))

(define nst (big 1000000))

(if (= (eval nst) 10000000)
  (write "Test passed")
  (write "Test failed"))
