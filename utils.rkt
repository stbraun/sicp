; Some utility functions.
(module utils racket
  (provide square
           expected
           timed-test
           timed-test-n
           print-statistics
           timed-tests
           average
           minimum
           maximum
           mean
           format-number)

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

  ; Measure runtime of a procedure by performing multiple runs.
  ; Returns statistics of cpu, wall, and gc times: 
  ; ((min mean max)(min mean max)(min mean max)).
  ; Times are in msec.
  (define (timed-test-n f ps n)
    ; Run a single test and return the performance data: (cpu wall gc)
    (define (run f ps)
      (let-values ([(v cpu wall gc) (time-apply f ps)])
        (list cpu wall gc)))
    ; Run the test n times and return a list of performance data: ((cpu wall gc) ...).
    (define (run-n f ps n)
      (cond ((<= n 0) '())
        (else (cons (run f ps) (run-n f ps (- n 1))))))
    (let* ([results (run-n f ps n)]
           [cpus (map first results)]
           [walls (map second results)]
           [gcs (map third results)])
      (list 
          (list (minimum cpus) (mean cpus) (maximum cpus))
          (list (minimum walls) (mean walls) (maximum walls))
          (list (minimum gcs) (mean gcs) (maximum gcs)))))

  (module+ test
    (require rackunit
             "factorial.rkt")
    (define (test-cpu-stats)
      (define test-args '(13333))
      (define test-n 3)
      (define test-message "timed-test-n - cpu time")
      (let* ([results (timed-test-n factorial-t test-args test-n)]
             [cpu (first results)]
             [cmin (first cpu)]
             [cmean (second cpu)]
             [cmax (third cpu)])
        (printf "~a cpu time statistics: min= ~amsec, mean= ~amsec, max= ~amsec~n" test-message cmin (format-number cmean 3) cmax)))
    (define (test-wall-stats)
      (define (test-func x) (sleep (* 0.001 x)))
      (define test-args '(100))
      (define test-n 3)
      (define test-message "timed-test-n - wall time")
      (let* ([results (timed-test-n test-func test-args test-n)]
             [wall (second results)]
             [wmin (first wall)]
             [wmean (second wall)]
             [wmax (third wall)])
        (printf "~a wall time statistics: min= ~amsec, mean= ~amsec, max= ~amsec~n" test-message wmin (format-number wmean 3) wmax)
        (check-true (>= wmin (first test-args)))
        (check-true (>= wmean wmin))
        (check-true (>= wmax wmean))))
    (test-cpu-stats)
    (test-wall-stats))

  ; Print statistics of test runs.
  ; Expects a title and a list of statistics for cpu, wall, and gc times: ((min mean max) ...).
  (define (print-statistics title stats)
    (define (print-statistic type stat)
      (printf "~a: min= ~amsec, mean= ~amsec, max= ~amsec~n" type (first stat) (format-number (second stat) 3) (third stat)))
    (printf "----- ~a -----~n" title)
    (for-each print-statistic (list " cpu" "wall" "  gc") stats))

  ; Measure runtime of a procedure in microseconds.
  ; Calculate mean over multiple runs.
  (define (timed-test-micros f ps n [warmup 1])
    (for ([i (in-range warmup)])
      (apply f ps))
    (define start-time (current-inexact-monotonic-milliseconds))
    (for ([i (in-range n)])
      (apply f ps))
    (let ([end-time (current-inexact-monotonic-milliseconds)])
      (/ (* (- end-time start-time) 1000.0) n)))

  ; Measure runtimes of a list of procedures.
  (define (timed-tests fs ps ms)
    (cond ((or (empty? fs) (empty? ps) (empty? ms)) "done")
      (else (timed-test (car fs) (car ps) (car ms))
            (timed-tests (cdr fs) (cdr ps) (cdr ms)))))

  (module+ test
    (require rackunit
             "factorial.rkt")
    (displayln "timed-tests")
    (timed-tests (list factorial-r factorial-t factorial-i) (list '(13333) '(13333) '(13333)) (list "     recursive" "tail-recursive" "     iterative")))

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
    (cond ((empty? l) 0.0)
      ((empty? (rest l)) (* 1.0 (first l)))
      (else (/ (foldl + 0.0 l) (length l)))))

  (module+ test
    (require rackunit)
    (check-equal? (mean '(2 4 6)) 4.0)
    (check-equal? (mean '(8 5 7 3)) 5.75)
    (check-equal? (mean '(57 44 88 66)) 63.75))

  ; Format a number to a specified number of decimal places.
  (define (format-number num decimal-places)
    (let ([multiplier (expt 10 decimal-places)])
      (/ (round (* num multiplier)) multiplier)))
    (module+ test
      (require rackunit)
      (check-equal? (format-number 3.14159 2) 3.14)
      (check-equal? (format-number 2.71828 3) 2.718)
      (check-equal? (format-number 1.61803 4) 1.6180))

) ;; end module
