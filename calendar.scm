; Calendrical calculations.

(module Calendar racket

        ; Determine the easter date for a given year using Spencer's algorithm.
        ; https://de.wikipedia.org/wiki/Spencers_Osterformel
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
          `(,day ,month ,year))

        (module+ test
                 (require rackunit)
                 (check-equal? (easter 2000) '(23 4 2000))
                 (check-equal? (easter 2021) '(4 4 2021))
                 (check-equal? (easter 2022) '(17 4 2022))
                 (check-equal? (easter 2023) '(9 4 2023))
                 (check-equal? (easter 2024) '(31 3 2024))
                 (check-equal? (easter 2025) '(20 4 2025))
                 (check-equal? (easter 2026) '(5 4 2026)))

        ; Determine the day of week for a given date using Zeller's algorithm.
        ; https://de.wikipedia.org/wiki/Wochentagsberechnung
        (define (day-of-week day month year)
          ; Adjust month and year for Zeller's algorithm 
          (define m-julian (if (< month 3) (+ month 10) (- month 2)))
          (define year-fixed (if (< month 3) (- year 1) year))
          (define y (modulo year-fixed 100))
          (define c (quotient year-fixed 100))
          (define wd (modulo (- (+ day (floor (- (* 13/5 m-julian) 1/5)) y (quotient y 4) (quotient c 4)) (* 2 c)) 7))  
          ;; Return the day of the week as a symbol.
          (list-ref (list 'sunday 'monday 'tuesday 'wednesday 'thursday 'friday 'saturday) wd))

        (module+ test
                 (require rackunit)
                 (check-equal? (day-of-week 1 1 1900) 'monday)
                 (check-equal? (day-of-week 4 2 1900) 'sunday)
                 (check-equal? (day-of-week 4 6 2006) 'sunday)
                 (check-equal? (day-of-week 12 6 2006) 'monday)
                 (check-equal? (day-of-week 1 1 2023) 'sunday)
                 (check-equal? (day-of-week 25 12 2023) 'monday)
                 (check-equal? (day-of-week 4 7 2023) 'tuesday)
                 (check-equal? (day-of-week 12 4 2025) 'saturday))

        ) ; end module
