; Exercise 1.11
; f is defined as follows:
; f(n) = n for n < 3
; f(n) = f(n-1) + 2(f(n-2) + 3f(n-3) for n >= 3
;
; Implement the function as a recursive and as an iterative process.

(define (fr n)
  (if (< n 3)
    n
    (+ (fr (- n 1)) (* 2 (fr (- n 2))) (* 3 (fr (- n 3))))))

(fr 1)
(fr 2)
(fr 3)
(fr 4)
(fr 5)
(fr 15)

(fr 5)
(+ (fr 4)                                                     (* 2 (fr 3))                               (* 3 (fr 2)))
(+ (fr 3)                           (* 2 (fr 2)) (* 3 (fr 1)) (* 2 (+ (fr 2) (* 2 (fr 1)) (* 3 (fr 0)))) (* 3 2))
(+ (fr 2) (* 2 (fr 1)) (* 3 (fr 0)) (* 2 2)      (* 3 1)      (* 2 (+ 2      (* 2 1)      (* 3 0)))      6)
(+ 2      (* 2 1)      (* 3 0)      4            3            (* 2 (+ 2      2            0))            6)
(+ 2      2            0            4            3            (* 2 4)                                    6)
25



