; Chapter 2 notes and exercises part 2

(require racket)


; --- 2.3 Symbolic Data
; ---------------------

(define (memq-x item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq-x item (cdr x)))))

(memq-j 'apple '(pear banana prune))  ; #f
(memq-x 'apple '(x (apple sauce) y apple banana pear))  ; (apple pear)

(member #'x (list #'x #'y))

(assoc 17 '((11 22) (12 24) (17 42) (18 36)))
(assoc 13.7 '((11 22) (12 24) (17 42) (18 36)) (lambda (v x) (< v x)))

