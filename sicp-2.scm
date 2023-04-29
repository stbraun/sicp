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

(display (map + (list 1 2 3) (list 2 4 6)))
(display (map (lambda (x y) (+ 1 (* x y))) (list 1 2 3) (list 2 4 6)))

; Exercise 2.23
; Implement for-each. The procedure applies a given function to each of the given items.
; It does not return any result.
(define (for-each f items)
  (cond ((not (null? items))
    (f (car items))
    (for-each f (cdr items)))))
        
(for-each (lambda (x) (newline) (display x)) (list 57 321 88))

; ------------

(define x (cons (list 1 2) (list 3 4)))

(length x)

; Count the leaves of a tree.
(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else
          (+ (count-leaves (car tree)) (count-leaves (cdr tree))))))

(count-leaves x)
(count-leaves (list x x))

; Exercise 2.25
; Give car / cdr combinations to pick the 7 from each of the following lists.

(define ll1 (list 1 3 (list 5 7) 9))
(define ll2 (list (list 7)))
(define ll3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(car (cdr (car (cdr ( cdr ll1)))))
(caar ll2)

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr ll3))))))))))))

; ------------

; Exercise 2.27
; Implement a deep-reverse procedure.
; (deep-reverse (list (list 1 2) (list 3 4))) --> ((4 3) (2 1))

(define (deep-reverse l)
  (if (pair? l)
    (reverse (map deep-reverse l)) 
    l))


(let ((ll (list (list 1 2) (list 3 4))))
  (display (deep-reverse ll)))

; ------------

; Exercise 2.30
; Square each node of a tree.

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (square-tree sub-tree)
           (* sub-tree sub-tree)))
       tree))

(display
(square-tree (list 1
                   (list 2 (list 3 4) 5)
                   (list 6 7)))
)

; ------------

; Exercise 2.31
; Abstract the procedure from 2.30 to work with any funktion.

(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (tree-map f sub-tree)
           (f sub-tree)))
       tree))

(define (square x) (* x x))

(display
(tree-map square (list 1
                   (list 2 (list 3 4) 5)
                   (list 6 7)))
)

; ------------

; --- Sequence Operations

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(display (filter even? (list 1 2 3 4 5 6)))

(define (accumulate op accumulator sequence)
  ; (display "accu: ") (display accumulator) (display ", seq: ") (display sequence) (newline)
  (if (null? sequence)
    accumulator
    (op (car sequence) 
        (accumulate op accumulator (cdr sequence)))))


(accumulate + 0 (list 10 20 30))
(accumulate * 1 (list 10 20 30))
(display (accumulate cons nil (list 10 20 30)))

; Provide an enumerator of integers over a given interval.
(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(display (enumerate-interval 2 7))

; Enumerate the leaves of a tree.
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))  ; a leaf
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(display (enumerate-tree (list 1 (list 2 (list 3 4)) 5)))

; Implement sum-odd-squares as signal flow in terms of our sequencde operations:
;
; [enumerate: tree leaves]->[filter: odd?]->[map: square]->[accumulate: +, 0]

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(sum-odd-squares (list 1 2 3 4 5))  ; 35

; Implement even-fibs as signal flow:
;
; [enumerate: integers]->[map: fib]->[filter: even?]->[accumulate: cons, ()]

(require "fibonacci.scm")

(define (even-fibs n)
  (accumulate cons
              nil
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

(display (even-fibs 8))

; Exercise 2.33

(define (map_ p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(display (map_ (lambda (x) (* 2 x)) (list 2 3 4)))

(define (append_ seq1 seq2)
  (accumulate cons seq2 seq1))

(display (append_ (list 1 2 3) (list 4 5 6)))

(define (length_ sequence)
  (accumulate (lambda (a b) (+ b 1)) 0 sequence))

(length_ (list 1 2 3 4))

; ------------

; Exercise 2.34
; Evaluate a polynomial using Horner's rule.
; Given a polynom:
; a_nx^n + a_{n-1}x^{n-1} + ... + a_1n^1 + a_0
; Horner's rule structures the computation as:
; (...(a_nx + a_{n-1}) x + ... + a_1) x + a_0
; For example, to compute: 1 + 3x + 5x^3 + x^5 at x=2 you would evaluate
; (horner-eval 2 (list 1 3 0 5 0 1))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) 
                (display this-coeff) (display " - ") (display higher-terms) (newline) 
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))


(+ 1 (* 3 2) (* 5 (expt 2 3)) (* 1 (expt 2 5)))  ; 79
(horner-eval 2 (list 1 3 0 5 0 1))  ; 79

; ------------

; Exercise 2.35
; Redefine count-leaves as an accumulation.
; (accumulate op acc sequence)

(define (count-leaves-a tree)
  (accumulate + 0 (map (lambda (t) 
                         1)
                       (flatten tree))))

(define lx (list (list 1 2) (list 3 4) 5))

(display (count-leaves-a lx))
(display (count-leaves-a (list)))

; ------------

; Exercise 2.36
; Implement accumulate-n. The procedure works like accumulate
; but it takes a sequence of sequences as third argument. 
; The sub-sequences must all be of the same length.
; accumulate-n combines all the first elements, all the second elements
; and so on and returns a sequence of the results.

(define (accumulate-n op initial seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op initial (map car seqs))
          (accumulate-n op initial (map cdr seqs)))))

(define seqs (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(accumulate-n + 0 seqs)

; ------------

; Exercise 2.37
; Provide some matrix operations.
; Suppose we represent vectors v = (v_i) as sequence of numbers,
; and matrices 0  (m_ij) as sequence of vectors (the rows of a matrix).
; For example, the matrix
; | 1 2 3 4 |
; | 4 5 6 6 |
; | 6 7 8 9 |
; is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)).

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose m)
  (accumulate-n cons nil m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

(let ((m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
      (n (list (list 1 2) (list 3 4) (list 5 6) (list 7 8)))
      (v (list 1 2 3 4)))
  (display (dot-product v (reverse v))) (newline)
  (display (matrix-*-vector m v)) (newline)
  (display (transpose m)) (newline)
  (display (matrix-*-matrix m n)) (newline))

; Exercise 2.38
; The accumulate procedure is also known as fold-right, 
; because it combines the first element of the sequence 
; with the result of combining all the elements to the right.
 is also a fold-left, which is similar to fold-right, 
; except it combines elements working in the opposite direction.

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))  ; 3/2
(/ 1 (fold-right / 1 (list 2 3)))
(/ 1 (/ 2 (fold-right / 1 (list 3))))
(/ 1 (/ 2 (/ 3 1)))
(/ 1 (/ 2 3))
(/ 3 2)

(fold-left / 1 (list 1 2 3))   ; 1/6
(fold-left / (/ 1 1) (list 2 3))
(fold-left / (/ (/ 1 1) 2) (list 3))
(fold-left / (/ (/ (/ 1 1) 2) 3) nil)
(/ (/ (/ 1 1) 2) 3)
(/ (/ 1 2) 3)
(/ 1 (* 2 3))
(/ 1 6)

(fold-right list nil (list 1 2 3))
(list 1 (fold-right list nil (list 2 3)))
(list 1 (list 2 (fold-right list nil (list 3))))
(list 1 (list 2 (list 3 nil)))

(fold-left list nil (list 1 2 3))
(fold-left list (list nil 1) (list 2 3))
(fold-left list (list (list nil 1) 2) (list 3))
(fold-left list (list (list (list nil 1) 2) 3) nil)
(list (list (list nil 1) 2) 3)

; fold-left and fold-right will produce the same result only
; if op is associative.
(fold-right + 111 (list 1 2 3))
(fold-left + 111 (list 1 2 3))

; ------------

; Exercise 2.39
; Implement reverse in terms of fold-left and fold-right.

(define (reverse-r seq)
  (fold-right (lambda (x y) (append y (list x))) nil seq))

(reverse-r (list 1 2 3))

(define (reverse-l seq)
  (fold-left (lambda (x y) (append (list y) x)) nil seq))

(reverse-l (list 1 2 3))

; ------------


