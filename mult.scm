; Multiplication by addition

(module mult racket
        (provide mult-lin
                 fast-mult-r
                 fast-mult)

        ; Exercise 1.17 - This implementation takes O(b) steps.
        (define (mult-lin a b)
          ;(printf "a: ~a, b: ~a\n" a b)
          (if (= b 0)
            0
            (+ a (mult-lin a (- b 1)))))

        (define (check-= expected actual)
          (printf "~a =!= ~a\n" expected actual))

        (module+ test
                 (require rackunit)
                 (check-= (* 13 7) (mult-lin 13 7) 0) 
                 (check-= (* 13 27) (mult-lin 13 27) 0))


        ; Now suppose we include, together with addition,
        ; operations double, which doubles an integer,
        ; and halve, which divides an (even) integer by 2.
        ; Using these, fast-mult-r uses only a logarithmic number of steps.

        (define (double i)
          (+  i i))

        (define (halve i)
          (if (even? i)
            (/ i 2)
            i))

        (define (fast-mult-r a b)
          (printf "a: ~a, b: ~a\n" a b)
          (cond ((= b 0) 0)
                ((even? b) (double (fast-mult-r a (halve b))))
                (else (+ a (fast-mult-r a (- b 1))))))

        (module+ test
                 (check-= (* 13 7) (fast-mult-r 13 7) 0)
                 (check-= (* 13 27) (fast-mult-r 13 27) 0)) 

        ; Provide a fast-mult creating an iterative process.

        (define (fast-mult a b)
          (iter a b 0))

        (define (iter a b acc)
          (printf "a: ~a, b: ~a -> acc: ~a\n" a b acc)
          (cond ((= b 0) acc)
                ((even? b) (iter (double a) (halve b) acc))
                (else (iter a (- b 1) (+ acc a)))))

        (module+ test
                 (check-= (* 13 7) (fast-mult 13 7) 0)
                 (check-= (* 13 27) (fast-mult 13 27) 0))
        )
