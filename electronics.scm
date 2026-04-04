; Some formulas for electronics calculations

(module electronics racket
        (provide parR
                 serC
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

) ; close module electronics
