; Sets and set related functions

; We define our set by specifying the operations we want to use on sets:
; union-set        - returns a set holding the combined elements of given sets
; intersection-set - determines the common elements of two given sets
; element-of-set?  - a predicate that determines whether a given element is contained in a given set
; adjoin-set       - adds an element to a set

; --- Sets as unordered lists.

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else
          (element-of-set? x (cdr set)))))

(element-of-set? 5 '(2 3 5 7))
(element-of-set? 4 '(2 3 5 7))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(adjoin-set 3 '(2 3 5 7))
(adjoin-set 4 '(2 3 5 7))


(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else
          (intersection-set (cdr set1) set2))))

(intersection-set '(1 2 3) '(2 3 4))
(intersection-set '(1 2 3) '(4 5 6))
(intersection-set '() '(4 5 6))
(intersection-set '(1 2 3) '())

(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else
          (cons (car set1)
                (union-set (cdr set1) set2)))))

(union-set '() '())
(union-set '() '(2 4 6))
(union-set '(1 3 5) '())
(union-set '(1 3 5) '(2 3 4))


