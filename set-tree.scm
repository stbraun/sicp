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


