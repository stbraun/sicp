; Exponentiation.

; Compute the exponential of a given number.
; the procedure takes as arguments a base b and a positive integer n and computes b^n.

; A recursive procedure that creates a linear recursive process.
; It requires O(n) steps and O(n) space.
(define (expt-r b n)
  (if (= n 0)
    1
    (* b (expt-r b (- n 1)))))

(expt-r 2 5)
; 32

; A recursive procedure creating a linear iterative process.
; It requires O(n) steps and O(1) space.
(define (expt-i b n)
  (expt-iter b 1.0 n))

(define (expt-iter b acc n)
  (if (= n 0)
    acc
    (expt-iter b (* acc b) (- n 1))))

(expt-i 2 5)

; We can compute the exponentiation with less steps by using successive squaring.
; E.g. b^8: b^2 = b*b; b^4= b^2 * b^2; b^8 = b^4 * b^4
; In general we can use the rule:
; b^n = (b^(n/2)^2  if n is even
; b^n = b * b^(n-1) if n is odd
;
; This process grows with O(log n) in steps and in space.
(define (fast-expt-r b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt-r b (/ n 2))))
        (else (* b (fast-expt-r b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(define (square n)
  (* n n))

(define (assert expected actual)
  (display (= expected actual)) (display ": ") 
  (display expected) (display " != ") (display actual) (newline))

(assert (expt 2 5) (fast-expt-r 2 5))
(assert (expt 3 4) (fast-expt-r 3 4))
(assert (expt 7 13) (fast-expt-r 7 13))

; Exercise 1.16 - A recursice procedure that creates an iterative process
; with O(log n) steps.
; Use that (b^(n/2)^2 = (b^2)^(n/2).
; Introduce a variable a such that the expression ab^n is constant from state to state.

(define (fast-expt-i b n)
  (fast-iter b n 1))

(define (fast-iter b n a)
  (trace-i b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-iter (square b) (/ n 2) a))
        (else (fast-iter b (- n 1) (* a b)))))

(define (trace-i b n a)
  (display "n = ") (display n) 
  (display ": b = ") (display b) 
  (display ", a = ") (display a) 
  (display " - a*b^n = ") (display (* a (expt b n))) (newline))

(assert (expt 2 5) (fast-expt-i 2 5))
(assert (expt 3 4) (fast-expt-i 3 4))
(assert (expt 7 13) (fast-expt-i 7 13))
