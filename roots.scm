; Finding roots of functions.

(require "utils.scm")

; Finding roots by the half-interval method

(define (search f neg-point pos-point)
  (define (close-enough? a b)
    (< (abs (- b a)) 0.0000001))
  (define (average a b)
    (/ (+ a b) 2))
  (let ((midpoint (average neg-point pos-point)))
    (printf "~a < ~a > ~a\n" neg-point midpoint pos-point)
    (if (close-enough? neg-point pos-point)
      midpoint
      (let ((test-value (f midpoint)))
        (cond ((positive? test-value)
               (search f neg-point midpoint))
              ((negative? test-value)
               (search f midpoint pos-point))
              (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else 
            (error "Values are not of opposite sign" a b)))))


(half-interval-method sin 2.0 4.0)

; Finding fixed points of functions
; A number x is called a ficex point of a functionf if x satisfies the equation f(x) = x. 
; For some functions f we can locate a fixed point by beginning with an initial guess and applaing f repeatedly.

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(fixed-point cos 1.0)
(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

; Exercise 1.35
; Compute the golden ratio by means of a fied-point procedure x -> 1 + 1/x.

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)


; Exercise 1.36
; Modify fixed-point to print the sequencce of approximations it generates.
; Then find the solution to x^x = 1000 finding a fixed point of x -> log(1000)/log(x).


(define (fp f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess count)
    (display count) (display ": ")
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next (+ count 1)))))
  (try first-guess 1))

(fp (lambda (x) (/ (log 1000) (log x))) 2.0)

; Exercise 1.37
; Write a continued fraction function.

(define (cont-frac n d k)
  (if (= k 0) (/ (n k) (d k))
    (/ (n k) (+ (d k) (cont-frac n d (- k 1))))))

; Approximate phi (golden ratio) using a continued fraction.
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)


