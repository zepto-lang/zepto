; This is an example for tail call elimination.
; It will run forever, but the memory it consumes stays constant (on my PC at around 3m of RAM).

(define (f) (g))
(define (g) (f))

(f)
