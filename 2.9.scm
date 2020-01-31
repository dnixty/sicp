(define (make-interval a b) (cons a b))

(define (upper-bound i)
  (cdr i))

(define (lower-bound i)
  (car i))

(define (width-interval i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; Let
;; w(i) - width of the interval
;; (lb, up) - new interval with lower bound lb, and upper bound ub
;; i1, i2 - intervals
;; lb1, ub1 - lower and upper bounds of i1
;; lb2, ub2 - lower and upper bounds of i2

;; To show that the wdith of the sum (or difference) of two intervals is
;; a function only of the widths of the intervals being added (subtracted)
;; we need to prove that
;; w(i1 + i2) = w(i1) + w(i2)
;; and
;; w(i1 - i2) = w(i1) + w(i2)

;; w(i1 + i2) = w(lb1+lb2, ub1+ub2) = ((ub1+ub2) - (lb1+lb2)) / 2
;; w(i1) + w(i2) = ((ub1-lb1) / 2) + ((ub2-lb1) / 2) =
;; = ((ub1+ub2) - (lb1+lb2)) / 2
;; L=P

;; w(i1 - i2) = w(lb1-ub2, ub1-lb2) = ((ub1-lb2) - (lb1-ub2)) / 2
;; w(i1) + w(i2) = ((ub1-lb1) / 2) + ((ub2-lb2) / 2) =
;; ((ub1+ub2) - (lb1+lb2)) / 2

;; Example
(define i1 (make-interval 2 4))
(define i2 (make-interval 3 15))
(width-interval (add-interval i1 i2)) ;7
(width-interval (sub-interval i1 i2)) ;7
(width-interval i1) ; 1
(width-interval i2) ; 6

;; For multiplication and division it doesn't hold because
w(i1 * i2) != w(i1) + w(i2)
(width-interval (mul-interval i1 i2)) ;27
