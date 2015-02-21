(define (list . objs)
  objs)

(define (id obj)
  obj)

(define (flip func)
  (lambda (arg1 arg2)
    (func arg2 arg1)))

(define (list-tail l k) "get tail of a list"
    (if (zero? k)
      l
      (list-tail (cdr l) (- k 1))))

(define (list-ref l k) (car (list-tail l k)))

(define (append i a) (foldr (lambda (ax ix) (cons ax ix)) a i))

(define (curry func arg1)
  (lambda (arg)
    (func arg1 arg)))

(define (compose f g)
  (lambda (arg)
    (f (apply g arg))))

(define zero?
  (curry = 0))

(define positive?
  (curry < 0))

(define negative? 
  (curry > 0))

(define (odd? num)
  (= (mod num 2) 1))

(define (even? num)
  (= (mod num 2) 0))

(define (foldr func end l)
  (if (null? l)
    end
    (func (car l) (foldr func end (cdr l)))))

(define (foldl func accum l)
  (if (null? l)
    accum
    (foldl func (func accum (car l)) (cdr l))))

(define (generate func init pred)
  (if (pred init)
    (cons init '())
    (cons init (unfold func (func init) pred))))

(define (sum . l)
  (fold + 0 l))

(define (product . l)
  (fold * 1 l))

(define (max first . l)
  (fold (lambda (old new)
                (if (> old new) old new))
        first
        l))

(define (min first . l)
  (fold (lambda (old new)
          (if (< old new) old new))
        first
        l))

(define (length l)
  (fold (lambda (x y)
                (+ x 1))
        0
        l))

(define (reverse l)
  (fold (flip cons) '() l))

(define (my-mem-helper obj lst cmp-proc)
    (cond
        ((null? lst) #f)
        ((cmp-proc obj (car lst)) lst)
        (else (my-mem-helper obj (cdr lst) cmp-proc))))

(define (memq obj lst) (my-mem-helper obj lst eq?))

(define (memv obj lst) (my-mem-helper obj lst eqv?))

(define (member obj lst) (my-mem-helper obj lst equal?))

(define (mem-helper pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))

(define (assq obj alist) (fold (mem-helper (curry eq? obj) car) #f alist))

(define (assv obj alist) (fold (mem-helper (curry eqv? obj) car) #f alist))

(define (assoc obj alist) (fold (mem-helper (curry equal? obj) car) #f alist))

(define (map func l)
  (foldr (lambda (x y)
           (cons (func x) y))
         '()
         l))

(define (filter pred l)
  (foldr (lambda (x y)
           (if (pred x)
               (cons x y)
               y))
         '()
         l))

(define (any? pred lst)
  (let any* ((l (map pred lst)))
    (cond
      ((null? l) #f)
      ((car l) #t)
      (else
        (any* (cdr l))))))

(define (every? pred lst)
  (let every* ((l (map pred lst)))
    (cond
      ((null? l) #t)
      ((car l)
       (every* (cdr l)))
      (else
        #f))))

(define all? every?)

(define (case x . cs) 
         if (== cs ())
            ("No Case Found")
            (if (== x (caar cs))  
                (cadar cs) 
                (unpack case (join (list x) (cdr cs)))))

(define head car)
(define tail list-tail)
