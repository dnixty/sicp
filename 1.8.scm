(define (cube-root x)
  (cube-root-iter 1 0 x))

(define (cube-root-iter guess old x)
  (if (good-enough? guess old)
      guess
      (cube-root-iter (improve guess x)
                 guess
                 x)))

(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* 2.0 guess))
     3.0))

(define (square x) (* x x))

(define (good-enough? guess old)
  (< (abs (- guess old)) (abs (* guess 0.001))))

(cube-root 1)
(cube-root -8)
(cube-root 27)
(cube-root -1000)
