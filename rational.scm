; Rational number arithmetic

(module rational racket
        (provide make-rat
                 numer
                 denom
                 add-rat
                 sub-rat
                 mul-rat
                 div-rat
                 equal-rat?
                 print-rat
                 printn-rat)

        (require "gcd.scm")

        ; Create a rational number and reduce it to lowest terms.
        (define (make-rat numerator denominator)
          (let ((g (gcd numerator denominator)))
            (cond ((= (abs (* numerator denominator)) (* numerator denominator))
                   (cons (abs (/ numerator g)) (abs (/ denominator g))))
                  ((< denominator 0)
                   (cons (/ (* -1 numerator) g) (/ (abs denominator) g)))
                  (else 
                    (cons (/ numerator g) (/ denominator g))))))

        (module+ test
                 (require rackunit)

                 ; Create rationals
                 (check-equal? (make-rat 1 2) (cons 1 2))
                 (check-equal? (make-rat 3 6) (cons 1 2))
                 (check-equal? (make-rat 3 6) (make-rat 1 2))
                 (check-equal? (make-rat -1 -2) (cons 1 2))
                 (check-equal? (make-rat 1 -2) (cons -1 2))
                 (check-equal? (make-rat -1 2) (cons -1 2))
                 )

        (define (numer r)
          (car r))

        (define (denom r)
          (cdr r))

        (module+ test
                 (define r1 (make-rat 2 5))
                 (define r2 (make-rat 3 5))
                 (define r3 (make-rat 1 3))

                 (check-equal? (numer r1) 2)
                 (check-equal? (denom r1) 5))

        (define (print-rat r)
          (display (numer r)) (display "/") (display (denom r)))

        (define (printn-rat r)
          (newline)
          (print-rat r))

        (define (add-rat r1 r2)
          (make-rat (+ (* (numer r1) (denom r2)) (* (numer r2) (denom r1))) (* (denom r1) (denom r2))))

        (define (sub-rat r1 r2)
          (make-rat (- (* (numer r1) (denom r2)) (* (numer r2) (denom r1))) (* (denom r1) (denom r2))))

        (define (mul-rat r1 r2)
          (make-rat (* (numer r1) (numer r2)) (* (denom r1) (denom r2))))

        (define (div-rat r1 r2)
          (make-rat (* (numer r1) (denom r2)) (* (numer r2) (denom r1))))

        (module+ test
                 ; Add
                 (check-equal? (add-rat r1 r2) (make-rat 1 1))
                 (check-equal? (add-rat r1 r3) (make-rat 11 15))

                 ; Subtract
                 (check-equal? (sub-rat r2 r1) (make-rat 1 5))
                 (check-equal? (sub-rat r1 r2) (make-rat -1 5))
                 (check-equal? (sub-rat r3 r1) (make-rat -1 15))

                 ; Multiply
                 (check-equal? (mul-rat r1 r3) (make-rat 2 15))

                 ; Divide
                 (check-equal? (div-rat r1 r3) (make-rat 6 5)))

        (define (equal-rat? r1 r2)
          (= (* (numer r1) (denom r2)) (* (numer r2) (denom r1))))

        (module+ test
                 ; Compare for equality
                 (check-true (equal-rat? r1 r1))
                 (check-false (equal-rat? r1 r2)))
        )
