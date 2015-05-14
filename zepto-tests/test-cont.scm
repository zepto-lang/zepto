(load "zepto-tests/unit.scm")

(define (f return)
    (return 2)
3)

(assert-equal (lambda () (f (lambda (x) x))) 3)
(assert-equal (lambda () (call/cc f)) 2)

(define (f return)
    (return (+ 1 2 3 (+ 4 5 6)))
3)

(assert-equal (lambda () (call/cc f)) (+ 1 2 3 4 5 6))
(assert-equal (lambda () (call-with-current-continuation f)) (+ 1 2 3 4 5 6))
(assert-equal (lambda () (call/cc procedure?)) #t)
(assert-equal (lambda () (call-with-current-continuation procedure?)) #t)

(define list-length
    (lambda (obj)
        (call-with-current-continuation
            (lambda (return)
                (letrec ((r
                    (lambda (obj)
                        (cond ((null? obj) 0)
                              ((pair? obj)
                               (+ (r (cdr obj)) 1))
                              (else (return #f))))))
            (r obj))))))

(unit-test-handler-results)
(unit-test-all-passed)
