; A sqrt implementation using lexical scoping to hide internal definitions.
; Since x is bound in the definition of sqrt, the internal procedures are in the scope of x.
; Thus, x can be used as a free variable in the internal definitions.

(require "utils.scm")

(define (sqrt x)
  (define (sqrt-iter guess last-guess)
    (if (good-enough? guess last-guess)
      guess
      (sqrt-iter (improve guess) guess)))
  (define (good-enough? guess last-guess)
    (< (abs (- (abs-error guess) (abs-error last-guess))) (/ x 10000000)))
  (define (abs-error guess)
    (abs (- (square guess) x)))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (average v1 v2)
    (/ (+ v1 v2) 2))
  (sqrt-iter 1.0 x))


; Test
(define (test-sqrt x)
  (abs (- (square (sqrt x)) x)))

(test-sqrt 0.00073)
(test-sqrt 81)
(test-sqrt 22355628474)

