;; All implementations here are "borrowed" from 
;; husk-scheme (github.com/justinethier/husk-scheme).
(define (force) "force execution of a delayed function"
    (lambda (object)
        (object)))

(define-syntax delay
    (syntax-rules ()
        ((delay expression)
            (make-promise (lambda () expression)))))
            
(define make-promise
    (lambda (proc)
        (let ((result-ready? #f)
                (result #f))
            (lambda ()
                (if result-ready?
                    result
                (let ((x (proc)))
                    (if result-ready?
                        result
                    (begin (set! result x)
                        (set! result-ready? #t)
                            result))))))))
