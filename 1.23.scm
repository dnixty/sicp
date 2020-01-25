(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes f t)
  (define (search-iter f)
    (if (< f t)
        (and (timed-prime-test f)
             (search-iter (+ f 2)))))
  (if (even? f)
      (search-iter (+ f 1))
      (search-iter f)))

(search-for-primes 100000000000 100000000100)
;; Previously: ~0.43
;; Now: ~0.28
;; Ratio: 1.54
(search-for-primes 1000000000000 1000000000100)
;; Previosuly: 1.36
;; Now: ~0.85
;; Ratio: 1.6

;; The ratio is not exactly two because with introducing `next'
;; we're adding additional if procedure which will skew the ratio
;; value to roughly 3:2
