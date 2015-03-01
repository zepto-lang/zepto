(load "scm-tests/skim-unit.scm")

(assert-equal (lambda () (help write)) "write to file")

(unit-test-handler-results)
(unit-test-all-passed)
