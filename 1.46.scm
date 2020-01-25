(define (iterative-improvement good-enough? improve)
  (lambda (first-guess)
    (define (iter guess)
        (if (good-enough? guess)
            guess
            (iter (improve guess))))
    (iter first-guess)))

(define (sqrt x)
  ((iterative-improvement
    (lambda (guess)
      (< (abs (- (square guess) x)) 0.001))
    (lambda (guess)
      (average guess (/ x guess))))
   1.0))

(sqrt 9)

(define (fixed-point f first-guess)
  ((iterative-improvement
    (lambda (guess)
      (< (abs (- guess (f guess))) 0.00001))
    (lambda (guess)
      (f guess)))
   first-guess))

(fixed-point cos 1.0)
