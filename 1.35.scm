;; Knowing that number x is called a "fixed point" of a function f
;; if x satisfies the equation f(x) = x, and that two quantities
;; a and b are said to be in the golden ration ϕ if (a+b)/a = a/b = ϕ. We have:
;;
;; (a+b)/a = a/b = ϕ
;; a/a + b/a = 1 + 1/ϕ
;; 1 + 1/ϕ = ϕ
;;
;; Thus we can see that ϕ is a fixed point of the transformation
;; x |-> 1 + 1/x

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
