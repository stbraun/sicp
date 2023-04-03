; Building abstractions with procedures.
; Using procedures as arguments and results.

(module abstrprocs racket
        (provide sum
                 sum-i
                 product
                 product-i
                 integral
                 simpsons-rule)

        (require "utils.scm")

        ; Often the same patterns shows up when writing procedures.
        ; For example, when summing up some values the skeleton of
        ; the procedures looks mostly the same: it checks for the base case,
        ; adds some expression to the next recursive call which is made
        ; with the next variable value.
        ; The expression to add and the calculation of the next value
        ; differ typically.
        ; A generic summing procedure can be written that takes 
        ; these differing parts as parameters.

        (define (sum term a next b)
          (if (> a b)
            0
            (+ (term a) (sum term (next a) next b))))


        (module+ test
                 (require rackunit)
                 (define (inc n) (+ n 1))

                 (define (cube x) (* x x x))

                 ; Sum the cubes of integers in range 2 to 4.
                 (check-equal? 99 (sum cube 2 inc 4))

                 ; If we want to sum a range of integers, we need an identity function for the term.
                 (define (identity x) x)

                 (check-equal? 55 (sum identity 1 inc 10))

                 ; We can also approximate pi using a series:
                 ; pi/8 = 1/(1*3) + 1/(5*7) + 1/(9*11) + ...  (Leibnitz)
                 (define (pi-sum a b)
                   (define (pi-term x)
                     (/ 1.0 (* x (+ x 2))))
                   (define (pi-next x)
                     (+ x 4))
                   (sum pi-term a pi-next b))

                 (check-= pi (* 8 (pi-sum 1 5000)) 0.001))

        ; The definite integral of a function f between the limits a and b can be approximated numerically using the formula:
        ; int_a^b f = [f(a + dx/2) + f(a + dx + dx/2) + f(a + 2dx + dx/2) + ...]dx
        ; for small values of dx.

        (define (integral f a b dx)
          (define (add-dx x) (+ x dx))
          (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))


        (module+ test
                 ; The exact value of the integral of cube between 0 and 1 is 1/4.
                 (check-= 0.25 (integral cube 0 1 0.01) 0.01)
                 (check-= 0.25 (integral cube 0 1 0.001) 0.001))

        ; Exercise 1.29
        ; Simpson's Rule is a more accurate method of numerical integration than the method illustrated above.
        ; Using Simpson's Rule, the integral of a function f between a and b is approximated as:
        ; h/3 * [y0 + 4*y1 + 2*y2 + 4*y3 + 2*y4 +...+ 2*y(n-2) + 4*y(n-1) + yn]
        ; where h = (b - a)/n, for some integer n, and yk = f(a + k*h).
        ; Increasing n increases the accuracy of the approximation.

        (define (simpsons-rule f a b n)
          (define h
            (/ (- b a) n))
          (define (k x)
            (round (/ (- x a) h)))
          (define (term x)
            (cond ((or (= x a) (= x b)) (f x))
                  ((even? (k x)) (* 2 (f x)))
                  (else (* 4 (f x)))))
          (define (next x)
            (+ x h))
          (* (/ h 3) (sum term a next b)))

        (module+ test
                 ; The exact integral of function cube between 0 and 1 is 1/4.
                 (check-= 0.25 (simpsons-rule cube 0 1.0 100) 0.01)
                 (check-= 0.25 (simpsons-rule cube 0 1.0 1000) 0.001)
                 (check-= 0.25 (simpsons-rule cube 0 1.0 10000) 0.0001)

                 ; measure runtime
                 (define (cube-int n)
                   (simpsons-rule cube 0 1.0 n))

                 (timed-test cube-int 100 "Integral of function cube between 0 and 1.0.")
                 (timed-test cube-int 1000 "Integral of function cube between 0 and 1.0.")
                 (timed-test cube-int 10000 "Integral of function cube between 0 and 1.0."))


        ; Exercise 1.30
        ; Write a version of sum that performs iteratively.

        (define (sum-i term a next b)
          (define (iter a result)
            (if (> a b)
              result
              (iter (next a) (+ result (term a)))))
          (iter a 0))

        (module+ test
                 (check-equal? 55 (sum-i identity 1 inc 10))
                 (check-equal? 3025 (sum-i cube 1 inc 10))

                 (timed-test (lambda (n) (sum cube 1 inc n)) 100 "sum cube 1 to n")
                 (timed-test (lambda (n) (sum cube 1 inc n)) 1000 "sum cube 1 to n")
                 (timed-test (lambda (n) (sum cube 1 inc n)) 10000 "sum cube 1 to n")

                 (timed-test (lambda (n) (sum-i cube 1 inc n)) 100 "sum-i cube 1 to n")
                 (timed-test (lambda (n) (sum-i cube 1 inc n)) 1000 "sum-i cube 1 to n")
                 (timed-test (lambda (n) (sum-i cube 1 inc n)) 10000 "sum-i cube 1 to n"))


        ; Exercise 1.31
        ; Write a procedure product analogous to sum.

        (define (product term a next b)
          (if (> a b)
            1
            (* (term a) (product term (next a) next b))))

        (module+ test
                 (check-equal? 120 (product identity 2 inc 5)))

        (define (product-i term a next b)
          (define (iter x result)
            (if (> x b)
              result
              (iter (next x) (* result (term x)))))
          (iter a 1))


        (module+ test
                 (define (factorial n)
                   (product-i identity 2 inc n))

                 (check-equal? 720 (factorial 6)))
        )
