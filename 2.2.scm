(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (midpoint-segment s)
  (define (average x y)
    (/ (+ x y) 2))
  (let ((x1 (x-point (start-segment s)))
        (y1 (y-point (start-segment s)))
        (x2 (x-point (end-segment s)))
        (y2 (y-point (end-segment s))))
    (make-point (average x1 x2)
                (average y1 y2))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define p1 (make-point 2 4))
(define p2 (make-point 4 8))
(define s (make-segment p1 p2))

(print-point (midpoint-segment s))
