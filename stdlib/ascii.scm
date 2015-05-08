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

;; The smiley was found on http://loveascii.com/smilies.html
(define (smiley) "be happy"
  (begin
    (display "    .-'\"\"\"\"\"'-.\n")
    (display "  .'           `.\n")
    (display " /   o      o    \\\n")
    (display ":                 :\n")
    (display "|                 |\n")
    (display ":   \\        /    :\n")
    (display " \\   `.____.'    /\n")
    (display "  `.           .'\n")
    (display "    `-._____.-'")))

(define (standard-fish . direction) "everyone ought to have one; or so I heard"
  (define (left-fish)
    (begin
      (display " _J\"\"-.\n")
      (display "/o )   \\ ,';\n")
      (display "\\ ,'    ;  /\n")
      (display " \"-.__.'\"\\_;")))

  (define (right-fish)
    (begin
      (display "     .-\"\"L_\n")
      (display ";`, /   ( o\\\n")
      (display "\\  ;    `, \/\n")
      (display ";_/\"`.__.-\"")))

  (define (fish-bubbles)
    (begin
      (display "              o O\n")
      (display "     .-\"\"L_ O  o\n")
      (display ";`, /   ( o\\  o\n")
      (display "\\  ;    `, \/\n")
      (display ";_/\"`.__.-\"")))

  (cond ((null? direction) (left-fish))
        ((eq? (car direction) :left) (left-fish))
        ((eq? (car direction) :right) (right-fish))
        ((eq? (car direction) :bubbles) (fish-bubbles))
        (else (display (list "Unrecognized option: " direction)))))

;; The goose was found on http://www.retrojunkie.com/asciiart/animals/ducks.htm
(define (non-standard-fish) "this might not be what you expect"
  (begin
    (display "                        __\n")
    (display "                      /` ,\\__\n")
    (display "                     |    ).-'\n")
    (display "                    / .--'\n")
    (display "                   / /\n")
    (display "     ,      _.==''`  \\\n")
    (display "   .'(  _.='         |\n")
    (display "  {   ``  _.='       |\n")
    (display "   {    \\`     ;    /\n")
    (display "    `.   `'=..'  .='\n")
    (display "      `=._    .='\n")
    (display "   jgs  '-`\\\\`__\n")
    (display "            `-._{\n")))
