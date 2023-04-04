; Finding roots of functions.

; Finding roots by the half-interval method

(define (search f neg-point pos-point)
  (define (close-enough? a b)
    (< (abs (- b a)) 0.0000001))
  (define (average a b)
    (/ (+ a b) 2))
  (let ((midpoint (average neg-point pos-point)))
    (printf "~a < ~a > ~a\n" neg-point midpoint pos-point)
    (if (close-enough? neg-point pos-point)
      midpoint
      (let ((test-value (f midpoint)))
        (cond ((positive? test-value)
               (search f neg-point midpoint))
              ((negative? test-value)
               (search f midpoint pos-point))
              (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else 
            (error "Values are not of opposite sign" a b)))))


(half-interval-method sin 2.0 4.0)
