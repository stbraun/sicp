; Some implementations of a factorial function.
(module factorial racket
        (provide factorial-r
                 factorial-t
                 factorial-i)

        ; Recursive approach.
        ; Both procedure and process aare recursive.
        ; The process is first expanding and than contracting again.
        ; This is a linear recursive process.
        (define (factorial-r n)
          (if (= n 1)
            1
            (* n (factorial-r (- n 1)))))

        (module+ test
                 (require rackunit)
                 (check-equal? 120 (factorial-r 5))
                 (check-equal? 720 (factorial-r 6))
                 (check-equal? 39916800 (factorial-r 11)))

        ; Tail-recursive approach.
        (define (factorial-t n)
          (define (fact-iter product counter)
            (if (> counter n)
              product
              (fact-iter (* counter product) (+ counter 1))))
          (fact-iter 1 1))

        (module+ test
                 (check-equal? 120 (factorial-t 5))
                 (check-equal? 720 (factorial-t 6))
                 (check-equal? 39916800 (factorial-t 11)))

        ; Iterative approach
        ; The procedure is recursive, but the process is iterative: 
        ; its state is captured competely by its three state variables, 
        ; and an interpreter need keep track of only three variables 
        ; in order to execute the process.
        ; This is a linear iterative process.
        (define (factorial-i n)
          (define (iter product counter)
            ; n is visible in iter as a free variable.
            (if (> counter n)
              product
              (iter (* product counter) (+ counter 1))))
          (iter 1 1))

        (module+ test
                 (check-equal? 120 (factorial-i 5))
                 (check-equal? 720 (factorial-i 6))
                 (check-equal? 39916800 (factorial-i 11)))
        )
