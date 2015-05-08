(define (f return)
     (return 2)
       3)
 
(display (f (lambda (x) x))) ; displays 3
 
(display (call-with-current-continuation f)) ; displays 2
