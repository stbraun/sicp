; Test for primality

(module primality racket
        (provide prime?
                 fast-prime?
                 fermat-test
                 timed-prime-test3
                 search-for-primes3
                 miller-rabin-test)

        (require "utils.scm"
                 sicp)

        ; This version searches the smallest integral divisor 
        ; greater than 1 of a given number n.
        ; It does this in a straightforward way, by testing n for divisibility 
        ; by successive integers starting with 2.
        ; The complexity is O(sqrt(n)) because we only have to test for numbers up
        ; to the square root of n (see the condition for (square test-divisor)).

        (define (smallest-divisor n)
          (find-divisor n 2))

        (define (find-divisor n test-divisor)
          (cond ((> (square test-divisor) n) n)
                ((divides? test-divisor n) test-divisor)
                (else (find-divisor n (+ test-divisor 1)))))

        (define (divides? a b)
          (= (remainder b a) 0))

        ; n is prime if and only if n is its own smallest divisor.
        (define (prime? n)
          (= n (smallest-divisor n)))


        (module+ test
                 (require rackunit)
                 (check-equal? #t (prime? 13))
                 (check-equal? #f (prime? 119))
                 (check-equal? #t (prime? 31))
                 (check-equal? #t (prime? 12379)))

        ; Fermat test: This test is based on Fermat's Little Theorem:
        ; > If n is a prime number and a is any positive integer less than n, 
        ; > then a raised to the nth power is congruent to a modulo n.
        ;
        ; Two numbers are said to be congruent modulo n 
        ; if they both have the same remainder when divided by n.
        ;
        ; If n is not prime, then, in general, 
        ; most of the numbers a < n will not satisfy the above condition.
        ;
        ; This leads to the following algorithm for testing primality:
        ; Given a number n, pick a random number a < n and compute the 
        ; remainder of a^n modulo n.
        ; If the result is not equal to a, then n is certainly not a prime number.
        ; If it is a, then chances are good that n is prime.
        ; Now pick another random number a and test with the same method.
        ; If it also satisfies the equation, then we can be even more confident
        ; that n is prime. By trying more and more values of a, we can increase
        ; our confidence in the result.

        (define (expmod base exp m)
          (cond ((= exp 0) 1)
                ((even? exp)
                 (remainder (square (expmod base (/ exp 2) m)) m))
                (else (remainder (* base (expmod base (- exp 1) m)) m))))

        (module+ test
                 (check-equal? 14 (expmod 14 17 17)))

        (define (fermat-test n)
          (define (try-it a)
            (= (expmod a n n) a))
          (try-it (+ 1 (random (- n 1)))))

        (module+ test
                 (check-equal? #t (fermat-test 17)))

        ; fast-prime runs the fermat-test a given number of times, 
        ; as specified by the parameter times.
        (define (fast-prime? n times)
          (cond ((= times 0) true)
                ((fermat-test n) (fast-prime? n (- times 1)))
                (else false)))

        (module+ test
                 (check-equal? #f (fast-prime? 119 19))
                 (check-equal? #t (fast-prime? 12379 21))
                 (check-equal? #t (fast-prime? 61 23))
                 (check-equal? #t (fast-prime? 561 21)))  ; this is a Carmichael number


        ; Exercise 1.21
        ;(smallest-divisor 199)    ; 199
        ;(smallest-divisor 1999)   ; 1999
        ;(smallest-divisor 19999)  ; 7

        ; Exercise 1.22
        ; A timed prime test function.

        (define (timed-prime-test n)
          (start-prime-test n (runtime)))

        (define (start-prime-test n start-time)
          (if (prime? n)
            (report-prime n (- (runtime) start-time)) (display "")))

        (define (report-prime n elapsed-time)
          (display n)
          (display "*** ")
          (display elapsed-time)
          (display "ms")
          (newline))

        ; Write a procedure that checks the primality of consecutive odd integers in a specified range.

        (define (search-for-primes start end)
          (cond ((even? start) (search-for-primes (+ start 1) end))
                ((< start end) (timed-prime-test start)
                               (search-for-primes (+ start 2) end))))

        ; Run the procedure to determine the first couple of primes 
        ; larger than 1000, 10000, 100000, and 1000000.
        ; Since our algorithm needs O(sqrt(n)) steps, the times to test for the primes 
        ; is expected to grow by about sqrt(10) from one range to the next.

        (search-for-primes 1000 1020)
        (search-for-primes 10000 10038)
        (search-for-primes 100000 100045)
        (search-for-primes 1000000 1000038)

        ; > (search-for-primes 1000 1020)
        ; 
        ; 1009*** 0
        ; 1013*** 1
        ; 1019*** 1
        ; > (search-for-primes 10000 10040)
        ; 
        ; 10007*** 2
        ; 10009*** 2
        ; 10037*** 1
        ; 10039*** 2
        ; > (search-for-primes 100000 100050)
        ; 
        ; 100003*** 5
        ; 100019*** 4
        ; 100043*** 5
        ; 100049*** 5
        ; > (search-for-primes 1000000 1000100)
        ; 
        ; 1000003*** 14
        ; 1000033*** 15
        ; 1000037*** 15
        ; 1000039*** 15
        ; 1000081*** 15
        ; 1000099*** 15


        ; Exercise 1.23
        ; Improve smallest-divisor to check for 2 and odd numbers only.


        ; n is prime if and only if n is its own smallest divisor.
        (define (prime2? n)
          (define (smallest-divisor n)
            (find-divisor n 2))
          (define (find-divisor n test-divisor)
            (cond ((> (square test-divisor) n) n)
                  ((divides? test-divisor n) test-divisor)
                  (else (find-divisor n (next test-divisor)))))
          (define (divides? a b)
            (= (remainder b a) 0))
          (define (next n)
            (if (= n 2) 
              3
              (+ n 2)))
          (= n (smallest-divisor n)))


        (define (timed-prime-test2 n)
          (define (start-prime-test n start-time)
            (if (prime2? n)
              (report-prime n (- (runtime) start-time)) (display "")))
          (define (report-prime n elapsed-time)
            (display n)
            (display "*** ")
            (display elapsed-time)
            (newline))
          (start-prime-test n (runtime)))


        (define (search-for-primes2 start end)
          (cond ((even? start) (search-for-primes2 (+ start 1) end))
                ((< start end) (timed-prime-test2 start)
                               (search-for-primes2 (+ start 2) end))))


        (search-for-primes2 1000 1020)
        (search-for-primes2 10000 10038)
        (search-for-primes2 100000 100045)
        (search-for-primes2 1000000 1000038)

        ; Exercise 1.24
        ; Repeat the same test using fast-prime?. This algorithm
        ; has O(log n) growth.

        (define (timed-prime-test3 n)
          (define (start-prime-test n start-time)
            (if (fast-prime? n 55)
              (report-prime n (- (runtime) start-time)) (display "")))
          (define (report-prime n elapsed-time)
            (display n)
            (display "*** ")
            (display elapsed-time)
            (newline))
          (start-prime-test n (runtime)))


        (define (search-for-primes3 start end)
          (cond ((even? start) (search-for-primes3 (+ start 1) end))
                ((< start end) (timed-prime-test3 start)
                               (search-for-primes3 (+ start 2) end))))


        (search-for-primes3 1000 1020)
        (search-for-primes3 10000 10038)
        (search-for-primes3 100000 100045)
        (search-for-primes3 1000000 1000038)

        ; > (search-for-primes3 1000 1020)
        ; 
        ; 1009*** 31
        ; 1013*** 32
        ; 1019*** 33
        ; > (search-for-primes3 10000 10038)
        ; 
        ; 10007*** 42
        ; 10009*** 42
        ; 10037*** 42
        ; > (search-for-primes3 100000 100045)
        ; 
        ; 100003*** 49
        ; 100019*** 49
        ; 100043*** 48
        ; > (search-for-primes3 1000000 1000038)
        ; 
        ; 1000003*** 56
        ; 1000033*** 56
        ; 1000037*** 58


        ; Exercise 1.27
        ; Demonstrate that the Carmichael  numbers fool the Fermat test.
        ; Write a procedure that takes an integer n and tests whether
        ; a^n is congruent to a modulo n for every a < n.
        ; Run this procedure with some well-known Carmichael numbers.

        (define (test-congruence-for-all-values-less-than-n n)
          (define (test-all a n)
            (cond ((= a 0) true)
                  ((= (expmod a n n) a) (test-all (- a 1) n))
                  (else false)))
          (test-all (- n 1) n))

        (module+ test
                 (check-equal? #t (test-congruence-for-all-values-less-than-n 13))
                 (check-equal? #f (test-congruence-for-all-values-less-than-n 15))
                 ; Test for Carmicheal numbers:
                 (check-equal? #t (test-congruence-for-all-values-less-than-n 561))
                 (check-equal? #t (test-congruence-for-all-values-less-than-n 1105))
                 (check-equal? #t (test-congruence-for-all-values-less-than-n 1729))
                 (check-equal? #t (test-congruence-for-all-values-less-than-n 2465))
                 (check-equal? #t (test-congruence-for-all-values-less-than-n 2821))
                 (check-equal? #t (test-congruence-for-all-values-less-than-n 6601)))


        ; Exercise 1.28
        ; Miller-Rabin test

        (define (miller-rabin-test n)
          (define (expmod base exp m)
            (define (remainder-square-checked x m)
              (if (and (not (or (= x 1)
                                (= x (- m 1))))
                       (= (remainder (square x) m) 1))
                0
                (remainder (square x) m)))
            (cond ((= exp 0) 1)
                  ((even? exp)
                   (remainder-square-checked (expmod base (/ exp 2) m) m))
                  (else (remainder (* base (expmod base (- exp 1) m)) m))))
          (define (try-it a)
            (= (expmod a (- n 1) n) 1))
          (try-it (+ 1 (random (- n 1)))))

        (module+ test
                 (check-equal? #t (miller-rabin-test 17))
                 (check-equal? #f (miller-rabin-test 119))
                 (check-equal? #f (miller-rabin-test 561))
                 (check-equal? #t (miller-rabin-test 31))
                 (check-equal? #f (miller-rabin-test 99)))
        )
