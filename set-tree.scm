; Sets represented by a binary tree.
; This reduces the effort for a search to O(log n) as long as the tree is balanced.
; 
; A node will be represented as a list with three elements: (entry, left, right).
; left and right are the shild nodes and may be '() indicating there is no node.

; Selectors

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

; Constructor
(define (make-tree entry left right)
  (list entry left right))

;; Set operations

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(element-of-set? 3 '(1 () ()))           ; #f
(element-of-set? 3 '(3 () ()))           ; #t
(element-of-set? 3 '(1 () (3 () ())))    ; #t
(element-of-set? 3 '(5 (3 () ()) '()))   ; #t
(element-of-set? 3 '(5 (1 () ()) '()))   ; #f

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(adjoin-set 3 '())
(adjoin-set 3 '(3 () ()))
(adjoin-set 3 '(1 () ()))
(adjoin-set 3 '(5 () ()))
(adjoin-set 3 '(1 () (2 () ())))
(adjoin-set 3 '(5 (2 () ()) ()))
(adjoin-set 3 '(5 (4 () ()) ()))

; Exercise 2.63

(define (tree->list-1 tree)
  ; (display "tree->list-1 ") (display tree) (newline)
  (if (null? tree)
    '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    ; (display "tree->list-2 ") (display tree) (display "; ") (display result-list) (newline)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))


(define tree-1 '(5 (3 (1 () (2 () ())) (4 () ())) (8 (6 () (7 () ())) (10 (9 () ()) ()))))
(define tree-2 '(1 () (2 () (3 () (4 () (5 () (6 () ())))))))

(tree->list-1 tree-1)
(tree->list-2 tree-1)

(tree->list-1 tree-2)
(tree->list-2 tree-2)

; -------------

; Exercise 2.64
; The procedure list->tree converts an ordered list to a balanced binary tree.
; The helper procedure partial-tree takes an integer n and a list of at least n elements 
; and constructs a balanced tree containing the first n elements of the list.
; The result returned by partial-tree is a pair whose car is the constructed tree 
; and whose cdr is the list of elements not included in the tree.

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts) right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))

(list->tree '( 1 3 5 7 9))
