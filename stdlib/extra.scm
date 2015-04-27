;; All definitions here are "borrowed" from
;; husk (github.com/justinethier/husk-scheme).
(define-syntax cond
    (syntax-rules (else =>)
        ((cond (else result1 result2 ...))
         ((lambda () result1 result2 ...))) 
        ((cond (test => result))
         (let ((temp test))
           (if temp (result temp))))
        ((cond (test => result) clause1 clause2 ...)
         (let ((temp test))
           (if temp
             (result temp)
             (cond clause1 clause2 ...))))
        ((cond (test)) test)
        ((cond (test) clause1 clause2 ...)
         (let ((temp test))
           (if temp
             temp
             (cond clause1 clause2 ...))))
        ((cond (test result1 result2 ...))
         (if test ((lambda () result1 result2 ...)))) 
        ((cond (test result1 result2 ...)
               clause1 clause2 ...)
         (if test
           ((lambda () result1 result2 ...)) 
           (cond clause1 clause2 ...)))))

(define-syntax case
  (syntax-rules (else =>)
                ((case (key ...)
                   clauses ...)
                 (let ((atom-key (key ...)))
                   (case atom-key clauses ...)))
                ((case key
                   (else => result))
                 (result key))
                ((case key
                   (else result1 result2 ...))
                 (if #t ((lambda () result1 result2 ...)))) 
                ((case key
                   ((atoms ...) result1 result2 ...))
                 (if (memv key '(atoms ...))
                   ((lambda () result1 result2 ...)))) 
                ((case key
                   ((atoms ...) => result)
                   clause clauses ...)
                 (if (memv key '(atoms ...))
                   (result key)
                   (case key clause clauses ...)))
                ((case key
                   ((atoms ...) result1 result2 ...)
                   clause clauses ...)
                 (if (memv key '(atoms ...))
                   ((lambda () result1 result2 ...)) 
                   (case key clause clauses ...)))))

(define-syntax when
  (syntax-rules ()
                ((when test result1 result2 ...)
                 (if test
                   (begin result1 result2 ...)))))

(define-syntax letrec*
  (syntax-rules ()
                ((letrec* ((var1 init1) ...) body1 body2 ...)
                 (let ((var1 #f) ...)
                   (set! var1 init1)
                   ...
                   (let () body1 body2 ...)))))

;; This is the only homebrew definition
(define-syntax unless
  (syntax-rules ()
                ((unless test result1 ...)
                 (if (not test)
                   result1
                   ...))))
