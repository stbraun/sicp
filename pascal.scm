; Pascal's triangle exercise 1.12

;          1
;        1   1
;      1   2   1
;    1   3   3   1
;  1   4   6   4   1

(define (pascal row col)
  (cond ((= row 1) 1)
        ((= col 1) 1)
        ((= row col) 1)
        (else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col)))))

(pascal 1 1)
(pascal 2 1)
(pascal 2 2)
(pascal 3 1)
(pascal 3 2)
(pascal 4 2)
(pascal 5 2)
(pascal 5 3)


