; GCD - Greatest Common Divider

(module gcd racket
        (provide gcd)

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

        (module+ test
                 (require rackunit)
                 (check-= 4 (gcd 16 12) 0)
                 (check-= 4 (gcd 12 16) 0)
                 (check-= 2 (gcd 206 40) 0)
                 (check-= 1 (gcd 23 49) 0)
                 (check-= 17 (gcd 34 119) 0)
                 )
        )
