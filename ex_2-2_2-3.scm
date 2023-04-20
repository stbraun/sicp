; Exercise 2.2
; Define data structures for line segments and points in a plane.
; Provide a function that computes the midpint of a line segment.

; A point consists of an x and a y coordinate.

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

; A line segment consists of a starting and an ending point.

(define (make-segment starting-point ending-point)
  (cons starting-point ending-point))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (length-segment s)
  (let* ((p-start (start-segment s))
         (p-end (end-segment s))
         (xs (x-point p-start))
         (ys (y-point p-start))
         (xe (x-point p-end))
         (ye (y-point p-end)))
  (sqrt (+ (square (- xe xs)) (square (- ye ys))))))

; Compute the midpoint of a given segment.

(define (midpoint s)
  (let* ((start (start-segment s))
         (end (end-segment s)))
    (make-point (/ (+ (x-point start) (x-point end)) 2) (/ (+ (y-point start) (y-point end)) 2))))


; Tests

(define (test-point)
  (printf "point(2,3) =!= ~v~n" (make-point 2 3))
  (printf "x of point(2,3) =!= ~v~n" (x-point (make-point 2 3)))
  (printf "y of point(2,3) =!= ~v~n" (y-point (make-point 2 3))))

(define (test-segment)
  (printf "segment( (1,2), (3,4)) =!= ~v~n" (make-segment (make-point 1 2) (make-point 3 4)))
  (printf "start of segment( (1,2), (3,4)) =!= ~v~n" (start-segment (make-segment (make-point 1 2) (make-point 3 4))))
  (printf "end of segment( (1,2), (3,4)) =!= ~v~n" (end-segment (make-segment (make-point 1 2) (make-point 3 4))))
  (printf "length of segment( (1,2), (3,4)) = 2.828... =!= ~v~n" (length-segment (make-segment (make-point 1 2) (make-point 3 4)))))

(define (test-midpoint)
  (printf "midpoint of segment( (1,2), (3,4)) = (2, 3) =!= ~v~n" (midpoint (make-segment (make-point 1 2) (make-point 3 4))))
  )


; Exercise 2.3
; Implement a representation of rectangles in a plane and provide
; functions that compute perimeter and area of a rectangle.

(define (make-rect p1 p2)
  (cons p1 p2))

(define (p1-rect r)
  (car r))

(define (p2-rect r)
  (cdr r))

(define (perimeter-rect r)
  (let* ((p1 (p1-rect r))
         (p2 (p2-rect r))
        (x1 (x-point p1))
        (y1 (y-point p1))
        (x2 (x-point p2))
        (y2 (y-point p2)))
   (* 2 (+ (abs (- x2 x1)) (abs (- y2 y1)))))) 

(define (area-rect r)
  (let* ((p1 (p1-rect r))
         (p2 (p2-rect r))
        (x1 (x-point p1))
        (y1 (y-point p1))
        (x2 (x-point p2))
        (y2 (y-point p2)))
   (* (abs (- x2 x1)) (abs (- y2 y1)))))


; Tests

(define (test-rect)
  (let* ((p1 (make-point 1 2))
         (p2 (make-point 4 6))
         (r (make-rect p1 p2)))
    (printf "Perimeter = 14 =!= ~a~n" (perimeter-rect r))
    (printf "Area = 12 =!= ~a~n" (area-rect r))))
