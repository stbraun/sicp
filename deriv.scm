; Symbolic Differentiation

; Differentiate a given expression for a given variable.
(define (deriv expr var)
  (cond ((constant? expr) 0)
        ((variable? expr) 
         (if (same-variable? expr var) 1 0))
        ((sum? expr)
         (make-sum (deriv (addend expr) var)
                   (deriv (augend expr) var)))
        ((product? expr)
         (make-sum
           (make-product (multiplier expr)
                         (deriv (multiplicand expr) var))
           (make-product (deriv (multiplier expr) var)
                         (multiplicand expr))))
        (else
          (error "unknown expression type -- DERIV" expr))))

; For the representation of algebraic expressions lists are used.
; ex. (+ x 3)

(define (constant? x)
  (number? x))

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        ((and (sum? a1) (sum? a2))
         (append a1 (cdr a2)))
        ((sum? a1)
         (append a1 (list a2)))
        ((sum? a2)
         (append (list '+ a1) (cdr a2)))
        (else
          (list '+ a1 a2))))

; Is expr a number of the given value?
(define (=number? expr num)
  (and (number? expr) (= expr num)))


(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
        (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        ((and (product? m1) (product? m2))
         (append m1 (cdr m2)))
        ((product? m1)
         (append m1 (list m2)))
        ((product? m2)
         (append (list '* m1) (cdr m2)))
        (else
          (list '* m1 m2))))

; A sum is a list whose first element is the symbol +.
(define (sum? x)
  (and (pair? x)
       (eq? (car x) '+)))

; The addend is the second element of the sum list.
(define (addend s)
  (cadr s))

; The augend is the third element of the sum list.
(define (augend s)
  (if (= (length (cddr s)) 1)
    (caddr s)
    (cons '+ (cddr s))))

; A product is a list whose first element is the symbol '*.
(define (product? x)
  (and (pair? x)
       (eq? (car x) '*)))

; the multiplier is the second element of a product list.
(define (multiplier p)
  (cadr p))

; The multiplicand is the third element of a product list.
(define (multiplicand p)
  (if (= (length (cddr p)) 1)
    (caddr p)
    (cons '* (cddr p))))

; Examples
(deriv '(+ x 3) 'x)

(deriv '(+ x 3 x) 'x)
(deriv '(+ x (+ 3 x)) 'x)
(deriv '(+ (* 2 x) 3) 'x)

(deriv '(* x y) 'x)

(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(* x y (+ x 3)) 'x)

(deriv '(* (* 2 x x x) (* 3 x) 5) 'x)

(deriv (deriv '(* x x x) 'x) 'x)
