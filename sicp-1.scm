; SICP chapter 1

(let ((trace #t))
    (display trace) (newline)
    (set! trace #f)
    (display trace) (newline)
  )

((lambda (x) (* x x)) (- 2 (* 4 3)))

(define (ff f x)
  (f x))

(define (square x) (* x x))
                   
(ff ´square 11)

(ff ´(lambda (x) (* x x)) 11)

