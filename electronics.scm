; Some formulas for electronics calculations

(module electronics racket
        (provide parR
                 serC
                 corner-freq-order1
                 )
(require "interval.scm")

;; Calculate the resistance resulting from two parallel resistors.
(define (parR r1 r2)
  (div-interval
   (mul-interval r1 r2)
   (add-interval r1 r2)))

;; Calculate the resulting capacitance of two serial capacitors.
(define (serC c1 c2)
  (div-interval
    (mul-interval c1 c2)
    (add-interval c1 c2)))

;; Corner frequency of a first-order RC LP or HP filter
(define (corner-freq-order1 R C)
  (div-interval (make-interval 1 1) (mul-interval (make-interval 2 2) (mul-interval (make-interval pi pi) (mul-interval R C)))))

) ; close module electronics
