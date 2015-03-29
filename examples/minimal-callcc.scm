(define (f return)
  (return 2)
  3)
 
(write (f (lambda (x) x)))
 
(write (call-with-current-continuation f))
(write (call/cc f))
