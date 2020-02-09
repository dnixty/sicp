(define (make-vector x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define make-segment cons)

(define start-segment car)

(define end-segment cdr)
