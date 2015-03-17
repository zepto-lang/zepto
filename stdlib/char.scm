(define (char-cmp? cmp a b) "compares two chars with a compare option cmp"
     (cmp (char->integer a) (char->integer b)))

(define (char-ci-cmp? cmp a b) "compares two chars case insensitive with a compare option cmp"
     (cmp (char->integer (char-downcase a)) (char->integer (char-downcase b)))) 

(define (char=? a b) "are chars equal" (char-cmp? = a b))
(define (char<? a b) "is char less than" (char-cmp? < a b))
(define (char>? a b) "is char greater than" (char-cmp? > a b))
(define (char<=? a b) "is char less than or equal to" (char-cmp? <= a b))
(define (char>=? a b) "is char greater than or equal to" (char-cmp? >= a b))

(define (char-ci=? a b) "are chars equal; case insensitive" 
  (char-ci-cmp? = a b))
(define (char-ci<? a b) "is char less than; case insensitive" 
  (char-ci-cmp? < a b))
(define (char-ci>? a b) "is char greater than; case insensitive" 
  (char-ci-cmp? > a b))
(define (char-ci<=? a b) "is char less than or equal to; case insensitive" 
  (char-ci-cmp? <= a b))
(define (char-ci>=? a b) "is char greater than or equal to; case insensitive" 
  (char-ci-cmp? >= a b))
