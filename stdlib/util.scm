(define (list . objs) "creates a list from objects"
  objs)

(define (id obj) "returns an object"
  obj)

(define (flip func) "flips two arguments for a function"
  (lambda (arg1 arg2)
    (func arg2 arg1)))

(define (indexed-tail l k) "get tail of a list starting at index"
    (if (zero? k)
      l
      (list-tail (cdr l) (- k 1))))

(define (list-ref l k) "get reference to list element at certain point"
    (car (list-tail l k)))

(define (list-tail l) "get tail of a list"
  (cdr l))

(define (append i a) "append something to a list"
    (foldr (lambda (ax ix) (cons ax ix)) a i))

(define (curry func arg1) "curry a function"
  (lambda (arg)
    (func arg1 arg)))

(define (compose f g) "compose two functions"
  (lambda (arg)
    (f (apply g arg))))

(define (foldr func end l) "fold right"
  (if (null? l)
    end
    (func (car l) (foldr func end (cdr l)))))

(define (foldl func accum l) "fold left"
  (if (null? l)
    accum
    (foldl func (func accum (car l)) (cdr l))))

(define (generate func init pred)
  (if (pred init)
    (cons init '())
    (cons init (unfold func (func init) pred))))

(define (sum . l) "sum of values"
  (fold + 0 l))

(define (product . l) "product of values"
  (fold * 1 l))

(define (max first . l) "maximum of values"
  (fold (lambda (old new)
                (if (> old new) old new))
        first
        l))

(define (min first . l) "minimum of values"
  (fold (lambda (old new)
          (if (< old new) old new))
        first
        l))

(define (length l) "length of list"
  (fold (lambda (x y)
                (+ x 1))
        0
        l))

(define (reverse l) "reverse list"
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

(define (map func l) "map function to list"
  (foldr (lambda (x y)
           (cons (func x) y))
         '()
         l))

(define (foreach func l) "apply function to each element on the list"
  (foldl (lambda (x y)
           (cons (func x) y))
         '()
         l))

(define (filter pred l) "filter list through preidcate"
  (foldr (lambda (x y)
           (if (pred x)
               (cons x y)
               y))
         '()
         l))

(define (any? pred lst) "does anything in the list satisfy the predicate?"
  (let any* ((l (map pred lst)))
    (cond
      ((null? l) #f)
      ((car l) #t)
      (else
        (any* (cdr l))))))

(define (every? pred lst) "do all values in the list satisfy the predicate?"
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

(define (iota n) "makes a list from numbers from 0 to n"
    (let ((acc '()))
      (do ((i 0 (+ i 1))) ((= i n)) (set! acc (append acc (list i)))) acc))

(define (unzip1-with-cdr . lists)
  (unzip1-with-cdr-iterative lists '() '()))

(define (unzip1-with-cdr-iterative lists cars cdrs)
  (if (null? lists)
      (cons cars cdrs)
      (let ((car1 (caar lists))
        (cdr1 (cdar lists)))
    (unzip1-with-cdr-iterative 
     (cdr lists) 
     (append cars (list car1))
     (append cdrs (list cdr1))))))

(define (for-each proc . lists) "applies a function to a bunch of arguments"
  (if (null? lists)
      (apply proc)
      (if (null? (car lists))
      #t
      (let* ((unz (apply unzip1-with-cdr lists))
         (cars (car unz))
         (cdrs (cdr unz)))
        (apply proc cars) (apply map (cons proc cdrs))))))
