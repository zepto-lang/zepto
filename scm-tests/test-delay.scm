(load "scm-tests/skim-unit.scm")

(assert-equal (lambda () (force (delay 1)))
			  1)
(define count 0)
(define x 50)
(define p (delay (begin (set! count (+ count 1))
                        (if (> count x)
                            count
                            (force p)))))

(assert-equal (lambda () (p)) 51)

(unit-test-handler-results)
(unit-test-all-passed)
