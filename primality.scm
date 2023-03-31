; Test for primality

(require "utils.scm")

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


(prime? 13)     ; #t
(prime? 119)    ; #f
(prime? 31)     ; #t
(prime? 12379)  ; #t

; Fermat test: This test is based on Fermat's Little Theorem:
; > If n is a prime number and a is any positive integer less than n, 
; > then a raised to the nth power is congruent to a modulo n.
; Two numbers are said to be congruent modulo n 
; if they both have the same remainder when divided by n.
; If n is not prime, then, in general, 
; most of the numbers a < n will not satisfy the above condition.
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

(expmod 14 17 17)  ; 14

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(fermat-test 17)  ; #t

; fast-prime runs the fermat-test a given number of times, 
; as specified by the parameter times.
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(fast-prime? 119 19)    ; #f
(fast-prime? 12379 21)  ; #t
(fast-prime? 61 23)     ; #t
(fast-prime? 561 21)    ; is not a prime but has the properties tested with the Fermat test -> Carmicheal number.


; Exercise 1.21
(smallest-divisor 199)    ; 199
(smallest-divisor 1999)   ; 1999
(smallest-divisor 19999)  ; 7

; Exercise 1.22
; A timed prime test function.

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display "*** ")
  (display elapsed-time))

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
(search-for-primes 10000 10040)
(search-for-primes 100000 100050)
(search-for-primes 1000000 1000100)

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
