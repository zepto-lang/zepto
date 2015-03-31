;;
;; Taken from(but modified):
;;
;; husk-scheme
;; http://github.com/justinethier/husk-scheme
;;
;; Written by Justin Ethier
;;
;; Test cases for SRFI-9 (Record Types)
;;

(load "scm-tests/unit.scm")
(load "stdlib/srfi/srfi-9.scm")

(define-record-type :pare
    (kons x y)
    pare?
    (x kar set-kar!)
    (y kdr))

(assert-equal (lambda () (pare? (kons 1 2))) #t)
(assert-equal (lambda () (pare? (cons 1 2))) #f)
(assert-equal (lambda () (kar (kons 1 2))) 1)
(assert-equal (lambda () (kdr (kons 1 2))) 2)
(assert-equal (lambda () (let ((k (kons 1 2)))
    (set-kar! k 3)
    (kar k)))
3)

(unit-test-handler-results)
(unit-test-all-passed)
