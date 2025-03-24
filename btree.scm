;; A binary tree data structure

(module btree racket
        (provide (all-defined-out))

        (define-struct node (value left right))

        ; (define (make-node value left right)
        ;   (node value left right))

        ; ;; make a leaf
        ; (define (make-leaf value)
        ;   (make-node value '() '()))

        ;; is leaf
        (define (leaf? node)
          (and (null? (node-left node))
               (null? (node-right node))))

        ;; is internal node
        (define (internal? node)
          (not (leaf? node)))

        ;; is root
        (define (root? node)
          (null? (node-left node))
          (null? (node-right node)))

        ;; is empty
        (define (empty? node)
          (and (null? (node-left node))
               (null? (node-right node))
               (null? (node-value node))))
        
        ;; traverse a tree in order
        (define (in-order node)
          (if (leaf? node)
              (list (node-value node))
              (append (in-order (node-left node))
                      (list (node-value node))
                      (in-order (node-right node)))))

        ;; map function over tree
        (define (map-tree f node)
          (if (leaf? node)
              (make-node (f (node-value node)) '() '())
              (make-node (f (node-value node))
                         (map-tree f (node-left node))
                         (map-tree f (node-right node)))))

        ) ;; end of module
