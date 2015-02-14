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
