; SICP chapter 1

(define (square x)
    (*  x x))

(square 7)
(square (square 3))

; Three variations of an abs function.
(define (abs1 x)
  (cond ((> x 0) x)
         ((= x 0) 0)
         ((< x 0) (- x))))

(define (abs2 x)
  (cond ((< x 0) (- x))
        (else x)))

(define (abs3 x)
  (if (< x 0)
    (- x)
    x))

(= (abs1 2) (abs2 2) (abs3 2))
(= (abs1 0) (abs2 0) (abs3 0))
(= (abs1 -3) (abs2 -3) (abs3 -3))


; Calculate the square root using Newton's method

(define (sqrt-iter guess last-guess x)
  (if (good-enough? guess last-guess x)
    guess
    (sqrt-iter (improve guess x) guess x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess last-guess x)
  (< (abs (- (abs-error guess x) (abs-error last-guess x))) (/ x 10000000)))

(define (abs-error guess x)
  (abs (- (square guess) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x x))

(sqrt 2)
(sqrt 0.0337)
(sqrt 3795620418)

(define x 0.19999770003)
(abs-error (sqrt x) x)

(define (square-log x)
  (exp (double (log x))))

(define (double x)
  (+ x x))

(square-log 3)
(square 3)
