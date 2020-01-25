(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

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

(search-for-primes 1000 1100)
;; Three smallest primes larger that 1000 are:
;; 1009, 1013 1019

(search-for-primes 10000 10100)
;; Three smallest primes larger that 10000 are:
;; 10007, 10009, 10037

(search-for-primes 100000 100100)
;; Three smallest primes larger than 100000 are:
;; 100003, 100019, 100043

(search-for-primes 1000000 1000100)
;; Three smallest primes larger than 1000000 are:
;; 1000003, 1000033, 1000037

(search-for-primes 100000000000 100000000100)   ;0.43
(search-for-primes 1000000000000 1000000000100) ;1.36
