; Exercise 1.15
; The sine of an angle specified in radians can be computed by
; making use of the approximation x sin x ≈ x if x is sufficiently small, 
; and the trigonometric identity
;   sin x = 3 sin x/3 - 4 sin^3 x/3
; to reduce the size of the argment of sin.
; For the purpose of this exercise an engle is considered "sufficiently small" 
; if its magnitude is not greater than 0.1 radians.

(define (cube x)
  (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))

(sine 12.15)

; Five calls to p are required 
(/ (/ (/ (/ (/ 12.15 3.0) 3.0) 3.0) 3.0) 3.0)
; 0.04999

; The angle needs to be devided by 3 n-times to get smaller than 0.1 radians:
; angle/(3^n) < 0.1  ->  angle < 0.1 3^n  ->  10 angle < 3^n  -> n > log_3(10 angle)
; So, the order of growth of the process is O(log_3 angle).

