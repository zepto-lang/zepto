(define len length)
(define fold foldl)
(define reduce fold)
(define ceil ceiling)
(define head car)
(define tail list-tail)

(define (ok? x) "is arg ok"
  (eq x :ok))

(define (error? x) "is arg error"
  (eq x :error))

(define (yes? x) "is arg yes"
  (eq x :yes))

(define (no? x) "is arg no"
  (eq x :no))
