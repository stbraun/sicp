; Fibonacci number.
(module fibonacci racket
        (provide fib
                 fib-2r
                 fib-i)

        ; recursive approach
        (define (fib-2r n)
          (cond ((= n 0) 0)
                ((= n 1) 1)
                (else (+ (fib-2r (- n 1)) (fib-2r (- n 2))))))

        (module+ test
                 (require rackunit)
                 (check-equal? 0 (fib-2r 0))
                 (check-equal? 1 (fib-2r 1))
                 (check-equal? 1 (fib-2r 2))
                 (check-equal? 2 (fib-2r 3))
                 (check-equal? 3 (fib-2r 4))
                 (check-equal? 5 (fib-2r 5))
                 (check-equal? 55 (fib-2r 10)))

        ; tail-recursive approach
        (define (fib-tr n)
          (define (fib-tail n a b)
            (if (= n 0)
              b
              (fib-tail (- n 1) (+ a b) a)))
          (fib-tail n 1 0))

        (module+ test
                 (require rackunit)
                 (check-equal? 0 (fib-tr 0))
                 (check-equal? 1 (fib-tr 1))
                 (check-equal? 1 (fib-tr 2))
                 (check-equal? 2 (fib-tr 3))
                 (check-equal? 3 (fib-tr 4))
                 (check-equal? 5 (fib-tr 5))
                 (check-equal? 55 (fib-tr 10)))

        ; iterative approach
        (define (fib-i n)
          (fib-iter 1 0 n))

        (define (fib-iter a b count)
          (if (= count 0)
            b
            (fib-iter (+ a b) a (- count 1))))

        (define fib fib-i)

        (module+ test
                 (check-equal? 0 (fib-i 0))
                 (check-equal? 1 (fib-i 1))
                 (check-equal? 1 (fib-i 2))
                 (check-equal? 2 (fib-i 3))
                 (check-equal? 3 (fib-i 4))
                 (check-equal? 5 (fib-i 5))
                 (check-equal? 55 (fib-i 10)))
        )
