; Sets as ordered lists

(module set-ord racket

        (provide element-of-set?
                 intersection-set
                 adjoin-set
                 union-set)

        (define (element-of-set? x set)
          (cond ((null? set) false)
                ((= x (car set)) true)
                ((< x (car set)) false)
                (else
                  (element-of-set? x (cdr set)))))

        (module+ test
                 (require rackunit)

                 (check-false (element-of-set? 3 '()))
                 (check-false (element-of-set? 3 '(1 2 4 5)))
                 (check-true (element-of-set? 4 '(1 2 3 4 5)))
                 (check-false (element-of-set? 7 '(1 2 3 4 5))))

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

        (module+ test
                 (check-equal? (intersection-set '() '()) '())
                 (check-equal? (intersection-set '() '(1 2 3)) '())
                 (check-equal? (intersection-set '(1 2 3) '()) '())
                 (check-equal? (intersection-set '(1 4 7) '(1 3 8)) '(1)))

        (define (adjoin-set x set)
          (cond ((null? set) (list x))
                ((< x (car set))
                 (cons x set))
                ((< (car set) x)
                 (cons (car set)
                       (adjoin-set x (cdr set))))
                ((= x (car set))
                 set)))

        (module+ test
                 (check-equal? (adjoin-set 3 '()) '(3))
                 (check-equal? (adjoin-set 3 '(4 5)) '(3 4 5))
                 (check-equal? (adjoin-set 3 '(1 4 5)) '(1 3 4 5))
                 (check-equal? (adjoin-set 3 '(1 2)) '(1 2 3))
                 (check-equal? (adjoin-set 3 '(1 2 3 4)) '(1 2 3 4)))

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

        (module+ test
                 (check-equal? (union-set '() '()) '())
                 (check-equal? (union-set '() '(1 2)) '(1 2))
                 (check-equal? (union-set '(0 1) '()) '(0 1))
                 (check-equal? (union-set '(1 2 5) '(1 3 4 6)) '(1 2 3 4 5 6)))
        )
