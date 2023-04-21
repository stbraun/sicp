; Interval arithmetic.

(module interval racket
        (provide make-interval
                 make-center-width
                 make-center-percent
                 lower-bound
                 upper-bound
                 center
                 percent
                 width
                 add-interval
                 sub-interval
                 mul-interval
                 div-interval
                 print-interval
                 printn-interval
                 print-center-width
                 printn-center-width
                 print-center-percent
                 printn-center-percent)

        (define (make-interval lower-bound upper-bound)
          (cons lower-bound upper-bound))

        (define (make-center-width c w)
          (make-interval (- c w) (+ c w)))

        (define (make-center-percent c p)
          (let ((w (/ (* c p) 100)))
            (make-interval (- c w) (+ c w))))

        (define (lower-bound i)
          (car i))

        (define (upper-bound i)
          (cdr i))

        (define (center i)
          (/ (+ (lower-bound i) (upper-bound i)) 2))

        (define (percent i)
          (* 100 (- 1 (/ (lower-bound i) (center i)))))

        (define (add-interval x y)
          (make-interval (+ (lower-bound x) (lower-bound y))
                         (+ (upper-bound x) (upper-bound y))))

        (define (sub-interval x y)
          (make-interval (- (lower-bound x) (upper-bound y))
                         (- (upper-bound x) (lower-bound y))))

        (define (mul-interval x y)
          (make-interval (* (lower-bound x) (lower-bound y))
                         (* (upper-bound x) (upper-bound y))))

        (define (div-interval x y)
          (let ((a (/ (lower-bound x) (upper-bound y)))
                (b (/ (upper-bound x) (lower-bound y))))
            (make-interval (min a b)
                           (max a b))))

        (define (width i)
          (/ (- (upper-bound i) (lower-bound i)) 2))


        (define (print-interval i)
          (printf "[~a, ~a]" (lower-bound i) (upper-bound i)))

        (define (printn-interval i)
          (print-interval i) (newline))

        (define (print-center-width i)
          (printf "~a ± ~a" (center i) (width i)))

        (define (printn-center-width i)
          (print-center-width i) (newline))

        (define (print-center-percent i)
          (printf "~a ± ~a%" (center i) (percent i)))

        (define (printn-center-percent i)
          (print-center-percent i) (newline))

        (module+ test
                 (require rackunit)

                 (check-equal? (make-interval 3 4) (cons 3 4))

                 (check-equal? (make-center-width 7 2) (cons 5 9))

                 (check-equal? (make-center-percent 20 10) (cons 18 22))

                 (define i1 (make-interval 2 3))
                 (define i2 (make-interval 4 5))
                 (define i3 (make-interval -4 4))

                 (check-equal? (lower-bound i1) 2)
                 (check-equal? (upper-bound i1) 3)

                 (check-equal? (add-interval i1 i2) (make-interval 6 8))
                 (check-equal? (sub-interval i1 i2) (make-interval -3 -1))

                 (check-equal? (mul-interval i1 i2) (make-interval 8 15))
                 (check-equal? (mul-interval i1 i3) (make-interval -8 12))

                 (check-equal? (div-interval i1 i2) (make-interval (/ 2 5) (/ 3 4)))
                 (check-equal? (div-interval i1 i3) (make-interval (/ -3 4) (/ 1 2)))

                 (check-equal? (width i1) (/ 1 2))
                 (check-equal? (width i3) 4)

                 (check-equal? (center i3) 0)
                 (check-equal? (center i1) (/ 5 2))

                 (check-equal? (percent (make-interval 18 22)) 10)
                 )

        )
