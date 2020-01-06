(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; a)
;; When (sine 12.15) is evaluated, the procedure p is applied
;; 5 times
;;
;; b)
;; The order of growth in space complexity is the number of
;; recursive calls of p when angle is greater than 0.1. Thus it's
;; Θ(log(angle))
;; The number of steps is of the same order as the order of growth
;; of space, which is Θ(log(angle))
