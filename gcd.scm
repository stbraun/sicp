; GCD - Greatest Common Divider

(require "utils.scm")

; Euclid's algorithm
; If r is the reainder when a is divided by b, 
; then the common divisors of a and b are precisely 
; the same as the common divisors of b and r.
; Thus, we can use the equation
; GCD(a,b) = GCD(b,r)

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(expected 4 (gcd 16 12))
(expected 4 (gcd 12 16))
(expected 2 (gcd 206 40))
(expected 1 (gcd 23 49))
(expected 17 (gcd 34 119))
