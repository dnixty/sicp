(define (make-vector x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vector (+ (xcor-vect v1)
                  (xcor-vect v2))
               (+ (ycor-vect v1)
                  (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vector (- (xcor-vect v1)
                  (xcor-vect v2))
               (- (ycor-vect v1)
                  (ycor-vect v2))))

(define (scale-vect v n)
  (make-vector (* n (xcor-vect v))
               (* n (ycor-vect v))))

(define v1 (make-vector 2 3))
(define v2 (make-vector 1 1))
(add-vect v1 v2)
(sub-vect v1 v2)
(scale-vect v1 2)
