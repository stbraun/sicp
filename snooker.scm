;; Some Snooker calculations.

(module snooker racket
        (provide points)

        ;; Calculate the maximum number of available points for a given number of red balls.
        (define (points reds)
          (cond ((or (< reds 0) (> reds 15)) "Invalid number of red balls")
                (else (+ 27 (* reds 8)))))

        (module+ test
                 (require rackunit)
                 (check-equal? (points 0) 27)
                 (check-equal? (points 1) 35)
                 (check-equal? (points 9) 99)
                 (check-equal? (points 15) 147)
                 (check-true (string? (points -1)))
                 (check-true (string? (points 16))))
        )
