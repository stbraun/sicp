; An implementation of Huffman encoding.
(module huffman racket
        (provide encode
                 decode)

        ; Leaf constructor and selectors

        (define (make-leaf symbol weight)
          (list 'leaf symbol weight))

        (define (leaf? object)
          (eq? (car object) 'leaf))

        (define (symbol-leaf x)
          (cadr x))

        (define (weight-leaf x)
          (caddr x))

        (module+ test
                 (require rackunit)

                 (check-equal? (make-leaf 'a 8) '(leaf a 8))
                 (check-equal? (leaf? (make-leaf 'a 8)) true)
                 (check-equal? (symbol-leaf (make-leaf 'a 8)) 'a)
                 (check-equal? (weight-leaf (make-leaf 'a 8)) 8)
                 )

        ; Node constructor and selectors.

        (define (make-code-tree left right)
          (list left
                right
                (append (symbols left) (symbols right))
                (+ (weight left) (weight right))))

        (define (left-branch tree)
          (car tree))

        (define (right-branch tree)
          (cadr tree))

        (define (symbols tree)
          (if (leaf? tree)
            (list (symbol-leaf tree))
            (caddr tree)))

        (define (weight tree)
          (if (leaf? tree)
            (weight-leaf tree)
            (cadddr tree)))

        (module+ test
                 (check-equal? (symbols (make-leaf 'a 8)) '(a))
                 (check-equal? (weight (make-leaf 'a 8)) 8)

                 (check-equal? (make-code-tree (make-leaf 'a 8) (make-leaf 'b 3)) '((leaf a 8) (leaf b 3) (a b) 11))
                 )

        ; Decoding a Huffman code
        ; Decode takes the encoded text as a list of 0 and 1 bits and the Huffman tree providing the encoding used.

        (define (decode bits tree)
          (define (decode-1 bits current-branch)
            (if (null? bits)
              '()
              (let ((next-branch (choose-branch (car bits) current-branch)))
                (if (leaf? next-branch)
                  (cons (symbol-leaf next-branch)
                        (decode-1 (cdr bits) tree))
                  (decode-1 (cdr bits) next-branch)))))
          (decode-1 bits tree))

        (define (choose-branch bit branch)
          (cond ((= bit 0) (left-branch branch))
                ((= bit 1) (right-branch branch))
                (else (error "Bad bit -- CHOOSE-BRANCH" bit))))

        ; Provide a set ordered by the weight of an item.
        (define (adjoin-set x set)
          (cond ((null? set) 
                 (list x))
                ((< (weight x) 
                    (weight (car set))) (cons x set))
                (else 
                  (cons (car set) 
                        (adjoin-set x (cdr set))))))

        ; Construct an initial ordered set of leaves based on a list of symbol-frequency pairs such as
        ; ((A 4) (B 2) (C 1) (D 1))
        (define (make-leaf-set pairs)
          (if (null? pairs)
            '()
            (let ((pair (car pairs)))
              (adjoin-set (make-leaf (car pair) 
                                     (cadr pair))
                          (make-leaf-set (cdr pairs))))))

        (module+ test
                 (check-equal? '() '())
                 (check-equal? (make-leaf-set '((A 4) (B 2) (C 1) (D 1))) '((leaf D 1) (leaf C 1) (leaf B 2) (leaf A 4)))
                 )

        (module+ test
                 ; Exercise 2.67
                 ; Define a sample encoding tree and a sample message.
                 ; Use decode to decode the message.
                 (define sample-tree
                   (make-code-tree (make-leaf 'A 4)
                                   (make-code-tree
                                     (make-leaf 'B 2)
                                     (make-code-tree
                                       (make-leaf 'D 1)
                                       (make-leaf 'C 1)))))

                 (define sample-message '( 0 1 1 0 0 1 0 1 0 1 1 1 0))

                 (check-equal? (decode sample-message sample-tree) '(A D A B B C A))

                 ; -------------
                 )

        ; Exercise 2.68
        ; Encode and decode a message using sample-tree above.

        ; Encode takes a message and a Huffman tree and produces a list of bits that gives the encoded message.
        (define (encode message tree)
          (if (null? message)
            '()
            (append (encode-symbol (car message) tree)
                    (encode (cdr message) tree))))

        (define (encode-symbol x tree)
          ; (display "encode-symbol ") (display x) (display "; ") (display tree) (newline)
          (define (encode-symbol-1 x branch encoded)
            ; (display "  encode-symbol-1 ") (display x) (display "; ") (display branch) (display " -> ") (display encoded) (newline)
            (cond ((leaf? branch) (reverse encoded))
                  ((member x (symbols (left-branch branch)))
                   (encode-symbol-1 x (left-branch branch) (cons 0 encoded)))
                  ((member x (symbols (right-branch branch)))
                   (encode-symbol-1 x (right-branch branch) (cons 1 encoded)))
                  (else
                    (error "Symbol not found!" x))))
          (cond ((null? tree) (error "Tree is empty!"))
                (else
                  (encode-symbol-1 x tree '()))))

        (module+ test
                 (check-equal? (encode-symbol 'A sample-tree) '(0))
                 (check-equal? (encode-symbol 'B sample-tree) '(1 0))
                 (check-equal? (encode-symbol 'C sample-tree) '(1 1 1))
                 (check-equal? (encode-symbol 'D sample-tree) '(1 1 0))

                 (check-equal? (encode '(A D A B B C A) sample-tree) sample-message)

                 (define msg '(B A D))
                 (check-equal? (decode (encode msg sample-tree) sample-tree) msg)
                 )

        )
