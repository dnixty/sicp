(define (make-interval a b) (cons a b))

(define (upper-bound i)
  (cdr i))

(define (lower-bound i)
  (car i))

(define (width-interval i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))

(define (spans-zero? i)
  (and (>= 0 (upper-bound i) 0)
       (<= (lower-bound i) 0)))

(define (div-interval x y)
  (if (spans-zero? y)
      (error "Can't divide by interval that spans zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))
