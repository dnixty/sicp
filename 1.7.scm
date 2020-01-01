(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))


;; For very small numbers good-enough? will not be very effective
;; because the guess is immedately accepted as the calculated
;; difference is always smaller than the initial value.
;; In (sqrt-iter 0.0005 0.001), good-enough? looks like this.
;; (good-enough? 0.0005 0.001)
;; (< (abs (- (square 0.0005) 0.001)) 0.001)
;; (< (abs (- 0.00000025 0.001) 0.001))
;; (< (abs -0.00099975) 0.001)
;; (< 0.00099975 0.001)
;; #t
;;
;; For very big numbers there is a chance that sqrt-iter will end up
;; as an infinite loop because the calculated difference will be too
;; small for machine precision to cope with. Thus, the guess will
;; never be good-enough? as the difference won't be smaller with each
;; iteration.

(define (sqrt-iter guess old x)
  (if (good-enough? guess old)
      guess
      (sqrt-iter (improve guess x)
                 guess
                 x)))

(define (good-enough? guess old)
  (< (abs (- guess old)) (* guess 0.001)))

(sqrt-iter 0.0005 0 0.001)

;; New version of sqrt-iter works better with big numbers because in
;; good-enought? the test is relative to the number.
