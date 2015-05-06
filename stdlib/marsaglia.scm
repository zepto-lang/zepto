; george marsaglia's random number generators,
; taken from http://programmingpraxis.codepad.org/sf8Z4pJP, edited slightly
; for testing the rngs, a test routine is included (test-rng).
; Testing might take a while, though, because do notation is still very slow.

(define (ipow b e)
  (cond ((zero? e) 1)
        ((even? e) (ipow (* b b) (/ e 2)))
        (else (* b (ipow (* b b) (/ (- e 1) 2))))))

(define (logand a b)
  (if (or (zero? a) (zero? b)) 0
    (+ (* (logand (floor (/ a 2)) (floor (/ b 2))) 2)
       (if (or (even? a) (even? b)) 0 1))))

(define (logxor a b)
  (cond ((zero? a) b)
        ((zero? b) a)
        (else
         (+ (* (logxor (floor (/ a 2)) (floor (/ b 2))) 2)
            (if (even? a)
                (if (even? b) 0 1)
                (if (even? b) 1 0))))))

(define (ash int cnt)
  (if (negative? cnt)
      (let ((n (ipow 2 (- cnt))))
        (if (negative? int)
            (+ -1 (quotient (+ 1 int) n))
            (quotient int n)))
      (* (ipow 2 cnt) int)))

(define mwc #f)
(define shr3 #f)
(define cong #f)
(define fib #f)
(define kiss #f)
(define lfib4 #f)
(define swb #f)
(define uni #f)
(define vni #f)
(define settable #f)

(let ((z 362436069) (w 521288629) (jsr 123456789)
      (jcong 380116160) (a 224466889) (b 7584631)
      (t (make-vector 256 0)) (x 0) (y 0) (c 0))

  (define (mod8 n) (modulo n 256))
  (define (mod32 n) (modulo n 4294967296))
  (define (ref i) (vector-ref t (mod8 i)))

  (set! mwc (lambda ()
    (set! z (mod32 (+ (* 36969 (logand z 65535)) (ash z -16))))
    (set! w (mod32 (+ (* 18000 (logand w 65535)) (ash w -16))))
    (mod32 (+ (ash z 16) w))))

  (set! shr3 (lambda ()
    (set! jsr (mod32 (logxor jsr (ash jsr 17))))
    (set! jsr (mod32 (logxor jsr (ash jsr -13))))
    (set! jsr (mod32 (logxor jsr (ash jsr 5)))) jsr))

  (set! cong (lambda ()
    (set! jcong (mod32 (+ (* 69069 jcong) 1234567))) jcong))

  (set! fib (lambda ()
    (set! b (mod32 (+ a b))) (set! a (mod32 (- b a))) a))

  (set! kiss (lambda ()
    (mod32 (+ (logxor (mwc) (cong)) (shr3)))))

  (set! lfib4 (lambda ()
    (set! c (mod8 (+ c 1)))
    (vector-set! t c (mod32 (+ (ref c) (ref (+ c 58))
      (ref (+ c 119)) (ref (+ c 178))))) (ref c)))

  (set! swb (lambda ()
    (set! c (mod8 (+ c 1)))
    (let ((bro (if (< x y) 1 0)))
      (set! x (mod32 (ref (+ c 34))))
      (set! y (mod32 (+ (ref (+ c 19)) bro)))
      (vector-set! t c (mod32 (- x y)))
      (vector-ref t c))))

  (set! uni (lambda ()
    (* (kiss) 2.328306e-10)))

  (set! vni (lambda ()
    (* (- (kiss) 2147483648) 4.6566133e-10)))

  (set! settable (lambda (i1 i2 i3 i4 i5 i6)
    (set! z i1) (set! w i2) (set! jsr i3) (set! jcong i4)
    (set! a i5) (set! b i6) (set! x 0) (set! y 0) (set! c 0)
    (do ((i 0 (+ i 1))) ((= i 256))
      (vector-set! t i (kiss))))))

(define-syntax rng-assert
  (syntax-rules ()
    ((rng-assert expr result)
      (if (not (equal? expr result))
          (write
            '("failed assertion: "
              "expected " result
              ", returned " expr))
            (write "test successful.")))))

(define (test-rng)
  (let ((k 0))
    (begin 
    (settable 12345 65435 34221 12345 9983651 95746118)
    (write "First test")
    (do ((i 0 (+ i 1))) ((= i 1e6) (rng-assert k 1064612766)) (set! k (lfib4)))
    (write "Second test")
    (do ((i 0 (+ i 1))) ((= i 1e6) (rng-assert k  627749721)) (set! k (swb)))
    (write "Third test")
    (do ((i 0 (+ i 1))) ((= i 1e6) (rng-assert k 1372460312)) (set! k (kiss)))
    (write "Fourth test")
    (do ((i 0 (+ i 1))) ((= i 1e6) (rng-assert k 1529210297)) (set! k (cong)))
    (write "Fifth test")
    (do ((i 0 (+ i 1))) ((= i 1e6) (rng-assert k 2642725982)) (set! k (shr3)))
    (write "Sixth test")
    (do ((i 0 (+ i 1))) ((= i 1e6) (rng-assert k  904977562)) (set! k (mwc)))
    (write "Seventh test")
    (do ((i 0 (+ i 1))) ((= i 1e6) (rng-assert k 3519793928)) (set! k (fib))))))
