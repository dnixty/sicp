(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (largest-power n b)
  (define (iter i n)
    (if (not (= (modulo n b) 0))
        i
        (iter (+ i 1) (/ n b))))
  (iter 0 n))

(define (car z)
  (largest-power z 2))

(define (cdr z)
  (largest-power z 3))

(car (cons 3 7))
(cdr (cons 2 5))
