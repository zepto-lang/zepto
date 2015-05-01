(define (read? x) "read from a file if it is an input file, else return #f"
  (if (input-port? x)
    (read x)
    #f))

(define (write? x s) "write s to a file if it is an output file, else return #f"
  (if (output-port? x)
    (write x s)
    #f))

(define (call-with-input-file s p) "open an input file s and apply a function to it, then close the file"
     (let ((inport (open-input-file s)))
          (if (eq? inport #f)
               #f
               (let ((res (p inport)))
                    (close-input-port inport)
                    res))))

(define (call-with-output-file s p) "open an output file s and apply a function to it, then close the file"
     (let ((outport (open-output-file s)))
          (if (eq? outport #f)
               #f
               (let ((res (p outport)))
                    (close-output-port outport)
                    res))))

(define (with-input-from-file s p) "open an input file s and run a function while it's open"
     (let ((inport (open-input-file s)))
          (if (eq? inport #f)
               #f
               (let ((prev-inport (current-input-port)))
                    (set-input-port inport)
                    (let ((res (p)))
                         (close-input-port inport)
                         (set-input-port prev-inport)
                         res)))))

(define (with-output-to-file s p) "open an output file s and run a function while it's open"
     (let ((outport (open-output-file s)))
          (if (eq? outport #f)
               #f
               (let ((prev-outport (current-output-port)))
                    (set-output-port outport)
                    (let ((res (p)))
                         (close-output-port outport)
                         (set-output-port prev-outport)
                         res)))))

