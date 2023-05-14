; Chapter 2 notes and exercises part 2

(require "interval.scm"
         "primality.scm"
         racket)


; --- 2.3 Symbolic Data
; ---------------------

(define (memq-x item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq-x item (cdr x)))))

(memq-x 'apple '(pear banana prune))  ; #f
(memq-x 'apple '(x (apple sauce) y apple pear))  ; (apple pear)



