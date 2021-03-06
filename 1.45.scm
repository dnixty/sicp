(define (power b p)
  (define (power-iter i r)
    (if (= i 0)
        r
        (power-iter (- i 1) (* r b))))
  (power-iter p 1))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (log2 x) (/ (log x) (log 2)))

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

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (nth-root x n)
  (fixed-point
   ((repeated average-damp (floor (log2 n)))
    (lambda (y) (/ x (power y (- n 1)))))
   1.0))

(nth-root 32 5)
