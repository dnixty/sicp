(define (make-accumulator n)
  (lambda (x)
    (set! n (+ n x))
    n))

;; Testing
(define A (make-accumulator 5))
(A 10)
(A 10)
