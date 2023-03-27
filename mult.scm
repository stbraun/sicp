; Multiplication by addition

(require rackunit)

; Exercise 1.17 - This implementation takes O(b) steps.
(define (mult-lin a b)
  ;(printf "a: ~a, b: ~a\n" a b)
  (if (= b 0)
    0
    (+ a (mult-lin a (- b 1)))))

(define (expected-equal expected actual)
  (printf "~a =!= ~a\n" expected actual))

(expected-equal (* 13 7) (mult-lin 13 7)) 
(expected-equal (* 13 27) (mult-lin 13 27)) 


; Now suppose we include, together with addition,
; operations double, which doubles an integer,
; and halve, which divides an (even) integer by 2.
; Using these, fast-mult uses only a logarithmic number of steps.

(define (double i)
  (+  i i))

(define (halve i)
  (if (even? i)
    (/ i 2)
    i))

(define (fast-mult a b)
  (printf "a: ~a, b: ~a\n" a b)
  (cond ((= b 0) 0)
        ((even? b) (+ (double (fast-mult a (halve b)))))
        (else (+ a (fast-mult a (- b 1))))))

(expected-equal (* 13 7) (fast-mult 13 7)) 
(expected-equal (* 13 27) (fast-mult 13 27)) 
