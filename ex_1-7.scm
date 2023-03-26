; Exercise 1.7
; Improve the termination condition of the square root function.

; Calculate the square root using Newton's method

(require "utils.scm")

(define (sqrt-iter guess last-guess x)
  (if (good-enough? guess last-guess x)
    guess
    (sqrt-iter (improve guess x) guess x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

; Define the termination condition based on the change of the guess after iterating.
(define (good-enough? guess last-guess x)
  (< (abs (- (abs-error guess x) (abs-error last-guess x))) (/ x 10000000)))

(define (abs-error guess x)
  (abs (- (square guess) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x x))


; Test
(define (test-sqrt x)
  (abs-error (sqrt x) x))

(test-sqrt 0.00073)
(test-sqrt 81)
(test-sqrt 22355628474)
