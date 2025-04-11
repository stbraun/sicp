
(module Calendar racket

    (define (easter year)
        (define a (modulo year 19))
        (define b (quotient year 100))
        (define c (modulo year 100))
        (define d (quotient b 4))
        (define e (modulo b 4))
        (define f (quotient (+ b 8) 25))
        (define g (quotient (- b (+ f 1)) 3))
        (define h (modulo (+ (* 19 a) b (- d) (- g) 15) 30))
        (define i (quotient c 4))
        (define k (modulo c 4))
        (define l (modulo (+ 32 (* 2 e) (* 2 i) (- h) (- k)) 7))
        (define m (quotient (+ a (* 11 h) (* 22 l)) 451))
        (define n (quotient (+ h l (- (* 7 m)) 114) 31))
        (define o (modulo (+ h l (- (* 7 m)) 114) 31))
        (define day (+ o 1))
        (define month n)
        ;; Return the date of Easter Sunday
        ;; as a list of the form '(month day year)
        `(,month ,day ,year))

    (module+ test
        (define (test-easter year expected)
            (let ((result (easter year)))
                (if (equal? result expected)
                    (printf "Test passed for year ~a: ~a\n" year result)
                    (printf "Test failed for year ~a: expected ~a, got ~a\n" year expected result))))
        ;; Test cases
        (test-easter 2000 '(4 23 2000))
        (test-easter 2021 '(4 4 2021))
        (test-easter 2022 '(4 17 2022))
        (test-easter 2023 '(4 9 2023))
        (test-easter 2024 '(3 31 2024))
        (test-easter 2025 '(4 20 2025))
        (test-easter 2026 '(4 5 2026)))

) ; end module
