(define (cowsay what) "what does the cow say?"

  (define (cow-interpolate-fill l what)
    (if (< l 24)
        (cow-interpolate-fill (+ l 1) (string-append what " "))
        what))

  (define (cow-interpolate what)
    (let ((l (string-length what)))
      (if (> l 24)
        (string-append 
          (string-append
            (string-append "| " (substring what 0 24))
            " |\n")
          (cow-interpolate (substring what 24 l)))
          (string-append 
            (string-append "| " 
              (cow-interpolate-fill l what))
            " |\n"))))

  (begin
    (display " _________________________\n")
    (display "/                         \\\n")
    (display (cow-interpolate what))
    (display "\\                         /\n")
    (display " -------------------------\n")
    (display "     \\   ^__^\n")
    (display "      \\  (oo)\\_______\n")
    (display "         (__)\\       )\\/\\\n")
    (display "             ||----w |\n")
    (display "             ||     ||")))


