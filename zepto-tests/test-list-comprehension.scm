(load "zepto-tests/unit.scm")

(assert-equal
    (lambda () [(+ x 1) | x <- '(1 2 3 4)])
    '(2 3 4 5))

(assert-equal
    (lambda () [(+ x 1) | x <- '(1 2 3 4), (> x 1)])
    '(3 4 5))

(unit-test-handler-results)
(unit-test-all-passed)
