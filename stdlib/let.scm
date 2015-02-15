;; All implementations here are "borrowed" from 
;; husk-scheme (github.com/justinethier/husk-scheme).
(define-syntax let
    (syntax-rules ()
        ((_ ((x v) ...) e1 e2 ...)
            ((lambda (x ...) e1 e2 ...) v ...))
        ((_ name ((x v) ...) e1 e2 ...)
            (let*
                ((f (lambda (name)
                    (lambda (x ...) e1 e2 ...)))
                (ff ((lambda (proc) (f (lambda (x ...) ((proc proc)
                        x ...))))
                    (lambda (proc) (f (lambda (x ...) ((proc proc)
                        x ...)))))))
                (ff v ...)))))

(define-syntax let*
    (syntax-rules ()
        ((_ () body) ((lambda () body)))
        ((_ ((var val)
                (vars vals) ...)
            body)
            (let ((var val))
                (let* ((vars vals) ...)
            body)))))

(define-syntax letrec
  (syntax-rules ()
                ((letrec ((var1 init1) ...) body ...)
                 (letrec "generate_temp_names"
                   (var1 ...)
                   ()
                   ((var1 init1) ...)
                   body ...))
                ((letrec "generate_temp_names"
                   ()
                   (temp1 ...)
                   ((var1 init1) ...)
                   body ...)
                 (let ((var1 #f) ...)
                   (let ((temp1 init1) ...)
                     (set! var1 temp1)
                     ...
                     body ...)))
                ((letrec "generate_temp_names"
                   (x y ...)
                   (temp ...)
                   ((var1 init1) ...)
                   body ...)
                 (letrec "generate_temp_names"
                   (y ...)
                   (newtemp temp ...)
                   ((var1 init1) ...)
                   body ...))))
