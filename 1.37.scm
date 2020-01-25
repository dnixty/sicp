;; a)
(define (cont-frac n d k)
  (define (recur i)
    (if (> i k)
        0
        (/ (n i) (+ (d i)
                    (recur (+ i 1))))))
  (recur 1))

(define (f k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             k))

(define (k-to-calc answer tolerance)
  (define (try i)
    (if (< (abs (- answer (f i))) tolerance)
        i
        (try (+ i 1))))
  (try 0))

(k-to-calc 0.6180 0.0001)

;; b)
(define (cont-frac n d k)
  (define (iter i res)
    (if (= i 0)
        res
        (iter (- i 1) (/ (n i) (+ (d i) res)))))
  (iter k 0))
