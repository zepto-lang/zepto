(define (cowsay what) "what does the cow say?"

  (define (cow-interpolate-fill l what)
    (if (< l 30)
        (cow-interpolate-fill (+ l 1) (string-append what " "))
        what))

  (define (cow-good-substring what)
    (let ((l (- (string-length what) 1))
          (x (string-ref what (- (string-length what) 1))))
      (if (> (string-find what #\space) -1)
        (if (char=? x #\space)
          what
          (cow-good-substring (substring what 0 l)))
        what)))

  (define (cow-interpolate what)
    (let ((x (cow-good-substring (substring what 0 30)))
          (l (string-length what))
          (xl (string-length (cow-good-substring (substring what 0 30)))))
      (if (> l  30)
        (string-append
          (string-append
            (string-append "|   "
              (cow-interpolate-fill xl x))
            "   |\n")
          (cow-interpolate (substring what xl l)))
        (string-append
          (string-append "|   "
            (cow-interpolate-fill l what))
          "   |\n"))))

  (begin
    (display " ___________________________________\n")
    (display "/                                   \\\n")
    (display (cow-interpolate what))
    (display "\\                                   /\n")
    (display " -----------------------------------\n")
    (display "     \\   ^__^\n")
    (display "      \\  (oo)\\_______\n")
    (display "         (__)\\       )\\/\\\n")
    (display "             ||----w |\n")
    (display "             ||     ||")))


