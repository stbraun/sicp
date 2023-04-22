; Chapter 2 notes and exercises


(define (new-rat numerator denominator)
   (cons numerator denominator))

(require "interval.scm")

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2) (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one (add-interval (div-interval one r1) (div-interval one r2)))))

(define r1 (make-center-percent 200 1.0))
(define r2 (make-center-percent 100 1.0))
(define r3 (make-center-percent 100 0.0))
(define r4 (make-center-percent 1 0.0))

(printn-center-percent (par1 r1 r2))
(printn-center-percent (par2 r1 r2))

(printn-center-percent (par1 r1 r3))
(printn-center-percent (par2 r1 r3))

(printn-center-percent (par1 r1 r4))
(printn-center-percent (par2 r1 r4))

(printn-center-percent (div-interval r2 r1))
(printn-center-percent (div-interval r2 r2))

(printn-center-percent (div-interval r4 r1))
(printn-center-percent (div-interval r4 r4))

(printn-center-percent (mul-interval r2 r1))
(printn-center-percent (mul-interval r2 r2))

(printn-center-percent (add-interval r2 r1))
(printn-center-percent (add-interval r2 r2))


(define ll (list 1 2 3 4))
(define l2 (list 1 2 (list 3 4) 5))
(define l3 (list 1 2 (list (list 3 4) (list 5 6)) 7))
(caaddr l2)
(cdr (car (cdr (cdr l3))))
(cdr (cdr (car (cdr (cdr l3)))))
(car (cdr (car (cdr (cdr l3)))))
(car (car (cdr (car (cdr (cdr l3))))))

(define (len l)
  (if (null? l) 0
    (+ 1 (len (cdr l)))))

(len (caddr l3))

(display (append l2 l3))


; Exercise 2.17

(define (last-pair l)
  ; (display l) (newline)
  (cond ((null? l) nil)
        ((= (len l) 1) l)
        (else (last-pair (cdr l)))))

(display (last-pair (list 23 72 149 34)))

; ------------

; Exercise 2.18

(define (reverse l)
  (if (null? l) nil
    (append (reverse (cdr l)) (list (car l)))))

(display (reverse (list 1 4 9 16 25)))

; ------------

; Exercise 2.19
; Coin change

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount (except-first-denomination coin-values))
             (cc (- amount (first-denomination coin-values)) coin-values)))))

(define (no-more? cv)
  (null? cv))

(define (except-first-denomination cv)
  (cdr cv))

(define (first-denomination cv)
  (car cv))

(display (cc 100 us-coins))
(display (cc 100 uk-coins))

; ------------

; Exercise 2.20

; Return a list of all arguments that have the same parity (even/odd) as the first argument.
(define (same-parity x . l)
  ; (display "==> (same-parity ") (display x) (display " . ") (display l) (display ")") (newline)
  (define (next-parity l)
    ; (display "    (next-parity ") (display l) (display ")") (newline)
    (if (null? l) l
      (cond ((even? x)
             (if (even? (car l)) l
               (next-parity (cdr l))))
            (else
              (if (odd? (car l)) l
                (next-parity (cdr l)))))))
  (define (same-parity-iter ll)
    ; (display " (same-parity-iter ") (display ll) (display ")") (newline)
    (if (null? ll) nil
      (cons (car ll) (same-parity-iter (next-parity (cdr ll))))))
  (same-parity-iter (cons x l)))


(display (same-parity  1 2 3 4 5 6))
(display (same-parity  2 3 4 5 6))

; ------------


