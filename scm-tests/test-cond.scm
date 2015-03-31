(load "scm-tests/unit.scm")

(assert-equal (lambda () (cond ((> 3 2) 'greater) ((< 3 2) 'less)))
			  'greater)
(assert-equal (lambda () (cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal)))
			  'equal)

(unit-test-handler-results)
(unit-test-all-passed)
