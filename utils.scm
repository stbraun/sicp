; Some utility functions.
(module utils racket
        (provide square
                 expected
                 timed-test
                 average)

        (require sicp)

        (define (square x)
          (*  x x))

        (define (expected ex act)
          (printf "~a =!= ~a\n" ex act))

        ; Measure runtime of a procedure.
        (define (timed-test f n message)
          (define (start-test n start-time)
            (define (report-time n elapsed-time)
              (display "n=")
              (display n)
              (display " *** ")
              (display elapsed-time)
              (display "msec")
              (display "  - ")
              (display message)
              (newline))
            (if (f n)
              (report-time n (- (runtime) start-time)) (display "")))
            (start-test n (runtime)))

        (define (average a b)
          (/ (+ a b) 2))
        )
