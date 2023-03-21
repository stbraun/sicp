; Tree recursion
; Fibonacci numbers
(define (fib-2r n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(fib-2r 0)
(fib-2r 1)
(fib-2r 2)
(fib-2r 3)
(fib-2r 4)
(fib-2r 5)

; iterative approach
(define (fib-i n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
    b
    (fib-iter (+ a b) a (- count 1))))

(fib-i 0)
(fib-i 1)
(fib-i 2)
(fib-i 3)
(fib-i 4)
(fib-i 5)
(fib-i 100)

