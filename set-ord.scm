; Sets as ordered lists

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else
          (element-of-set? x (cdr set)))))

(element-of-set? 3 '())
(element-of-set? 3 '(1 2 4 5))
(element-of-set? 3 '(1 2 3 4 5))
(element-of-set? 7 '(1 2 3 4 5))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1 (intersection-set (cdr set1) (cdr set2))))
            ((< x1 x2)
             (intersection-set (cdr set1) set2))
            ((< x2 x1)
             (intersection-set set1 (cdr set2)))))))

(intersection-set '() '())
(intersection-set '() '(1 2 3))
(intersection-set '(1 2 3) '())
(intersection-set '(1 4 7) '(1 3 8))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< x (car set))
         (cons x set))
        ((< (car set) x)
         (cons (car set)
               (adjoin-set x (cdr set))))
        ((= x (car set))
         set)))

(adjoin-set 3 '())
(adjoin-set 3 '(4 5))
(adjoin-set 3 '(1 4 5))
(adjoin-set 3 '(1 2))
(adjoin-set 3 '(1 2 3 4))

(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2)
                   (cons x1 (union-set (cdr set1) (cdr set2))))
                  ((< x1 x2)
                   (cons x1 (union-set (cdr set1) set2)))
                  ((> x1 x2)
                   (cons x2 (union-set set1 (cdr set2)))))))))

(union-set '() '())
(union-set '() '(1 2))
(union-set '(0 1) '())
(union-set '(1 2 5) '(1 3 4 6))


