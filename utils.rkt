; Some utility functions.
(module utils racket
  (provide square
           expected
           timed-test
           timed-tests
           average)

  (require sicp
           rackunit)


  (define (square x)
    (*  x x))

  (define (expected ex act)
    (printf "~a =!= ~a\n" ex act))

  ; Measure runtime of a procedure.
  (define (timed-test f n message)
    (define (start-test n start-time)
      (define (report-time n elapsed-time)
        (display "n=")
        (display n)
        (display " *** ")
        (display elapsed-time)
        (display "µsec")
        (display "  - ")
        (display message)
        (newline))
      (if (f n)
          (report-time n (- (runtime) start-time)) (display "")))
    (start-test n (runtime)))

  (define (timed-tests fs ps ms)
    (cond ((or (empty? fs) (empty? ps) (empty? ms)) "done")
      (else (timed-test (car fs) (car ps) (car ms))
            (timed-tests (cdr fs) (cdr ps) (cdr ms)))))

  (module+ test
    (require "factorial.rkt")
    (timed-tests (list factorial-r factorial-t) (list 7 7) (list "f-r" "f-t")))

    (define (average a b)
      (/ (+ a b) 2))
    )
