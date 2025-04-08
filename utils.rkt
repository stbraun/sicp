; Some utility functions.
(module utils racket
  (provide square
           expected
           timed-test
           timed-tests
           average)

  (define (square x)
    (*  x x))

  (module+ test
    (require rackunit)
    (check-equal? (square 2) 4)
    (check-equal? (square 3) 9)
    (check-equal? (square 44) 1936))

  (define (expected ex act)
    (printf "~a =!= ~a\n" ex act))

  ; Measure runtime of a procedure.
  (define (timed-test f ns message)
    (let-values ([(v cpu wall gc) (time-apply f ns)])
      (printf "~a ~a: cpu= ~amsec, wall= ~a, gc= ~a~n" message ns cpu wall gc)))

  (define (timed-tests fs ps ms)
    (cond ((or (empty? fs) (empty? ps) (empty? ms)) "done")
      (else (timed-test (car fs) (car ps) (car ms))
            (timed-tests (cdr fs) (cdr ps) (cdr ms)))))

  (module+ test
    (require "factorial.rkt")
    (displayln "Time calls to factorial:")
    (timed-tests (list factorial-r factorial-t factorial-i) (list '(33333) '(33333) '(33333)) (list "     recursive" "tail-recursive" "     iterative")))

    (define (average a b)
      (/ (+ a b) 2))

    (module+ test
      (require rackunit)
      (check-equal? (average 2 4) 3)
      (check-equal? (average 3 5) 4)
      (check-equal? (average 44 66) 55))

    (define (minimum l)
      (cond ((empty? l) 0)
        ((empty? (rest l)) (first l))
        (else (foldl min (first l) (rest l)))))

    (module+ test
      (require rackunit)
      (check-equal? (minimum '(2 4 6)) 2)
      (check-equal? (minimum '(8 5 7 3)) 3)
      (check-equal? (minimum '(57 44 66 88)) 44))
      
    (define (maximum l)
      (cond ((empty? l) 0)
        ((empty? (rest l)) (first l))
        (else (foldl max (first l) (rest l)))))

    (module+ test
      (require rackunit)
      (check-equal? (maximum '(2 4 6)) 6)
      (check-equal? (maximum '(8 5 7 3)) 8)
      (check-equal? (maximum '(57 44 88 66)) 88))

    (define (mean l)
      (cond ((empty? l) 0)
        ((empty? (rest l)) (first l))
        (else (/ (foldl + 0 l) (length l)))))

    (module+ test
      (require rackunit)
      (check-equal? (mean '(2 4 6)) 4)
      (check-equal? (mean '(8 5 7 3)) 23/4)
      (check-equal? (mean '(57 44 88 66)) 255/4))
    ) ;; end module
