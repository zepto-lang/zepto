(load "zepto-tests/unit.scm")

(assert-equal (lambda () (id `(list ,(+ 1 2) 4)))
    '(list 3 4))

; Test case from R5RS
(assert-equal (lambda () (let ((name 'a)) (id `(list ,name ',name))))
    '(list a (quote a)))

(assert-equal (lambda () (let ((name 'a)) (id `(list ,name (,name)))))
    '(list a (a)))

(assert-equal (lambda () (let ((name 'a)) (id `(list ,name ((,name))))))
    '(list a ((a))))

(assert-equal (lambda () (let ((name 'a)) (id `(list ,name . ,name))))
    '(list a . a))

(assert-equal (lambda () (let ((name 'a)) (id `(list ,name . (,name)))))
    '(list a a))

(assert-equal (lambda () (let ((name 'a)) (id `(list ,name . ((,name))))))
    '(list a (a)))

(assert-equal (lambda () (let ((name 'a)) (id `(list ,name . (,name . ,name)))))
    '(list a a . a))

(assert-equal (lambda () (id `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)))
    '(a 3 4 5 6 b))

(assert-equal (lambda () (id `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))))
    '((foo 7) . cons))

(assert-equal (lambda () (id `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)))
    '#(10 5 2.0 4.0 3.0 8))

(assert-equal (lambda () (id `(1 2 . ,(list 3 4))))
    '(1 2 3 4))

(assert-equal (lambda () (id (quasiquote (list (unquote (+ 1 2)) 4))))
    '(list 3 4))

(assert-equal (lambda () (id '(quasiquote (list (unquote (+ 1 2)) 4))))
    '`(list ,(+ 1 2) 4))

(assert-equal (lambda () (eval [+ 1 2])) 3)

(unit-test-handler-results)
(unit-test-all-passed)
