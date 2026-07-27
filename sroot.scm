; Square root calculation.
(module sroot racket
        (provide sroot
                 sroot-6)

        ; Calculate the square root of m. Start with a value of x and recurr until the presision is better than p.
        (define (sroot m x p)
          (cond ((< m 0) "Only positive values are supported.")  ; no complex results
                ((< (abs (- m (* x x))) p) (real->double-flonum x))  ; result found
                (else (sroot m (/ (+ x (/ m x)) 2) p))))         ; refine result

        (module+ test
                 (require rackunit)
                 (check-equal? (sroot -2 1 1) "Only positive values are supported.")
                 (check-= 42 (expt (sroot 42 1 1e-9) 2) 1e-9)
                 (check-= 51 (expt (sroot 51 1 1e-7) 2) 1e-7)
        )

        ; Square root with a precision of at least 1e-6. 
        (define (sroot-6 m)
          (sroot m 1 1e-6))

        (module+ test
                 (check-equal? (sroot-6 -42) "Only positive values are supported.")
                 (check-true (< (abs (- 51 (expt (sroot-6 51) 2))) 1e-6))
        )
)
