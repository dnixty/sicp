(define (cont-frac n d k)
  (define (iter i res)
    (if (= i 0)
        res
        (iter (- i 1) (/ (n i) (+ (d i) res)))))
  (iter k 0))

(define (e k)
  (+ (cont-frac
      (lambda (i) 1.0)
      (lambda (i)
        (if (= (modulo (+ i 1) 3) 0)
            (* 1.0 (expt 2 (/ (+ i 1) 3)))
            1.0))
      k)
     2))
