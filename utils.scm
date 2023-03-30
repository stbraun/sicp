; Some utility functions.
(module utils racket
        (provide square
                 expected)

        (define (square x)
          (*  x x))

        (define (expected ex act)
            (printf "~a =!= ~a\n" ex act))
        )
