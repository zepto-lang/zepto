(define (f return)
  (return 2)
  3)
 
(write (f (lambda (x) x))) ; displays 3
 
(write (call-with-current-continuation f)) ; displays 2
(write (call/cc f)) ; displays 2
