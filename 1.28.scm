(define (expmod base exp m)
   (cond ((= exp 0) 1)
         ((even? exp)
          (let ((x (expmod base (/ exp 2) m)))
            (if (non-trivial? x m) 0 (remainder (square x) m))))
         (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (non-trivial? n m)
  (cond ((= n 1) #f)
        ((= n (- m 1)) #f)
        (else (= (remainder (square n) m) 1))))

(define (miller-rabin-test a n)
  (cond ((= a 0) #t)
        ((= (expmod a (- n 1) n) 1) (miller-rabin-test (- a 1) n))
        (else #f)))

(define (miller-rabin n)
  (miller-rabin-test (- n 1) n))

(miller-rabin 100)

;; Prime numbers
(miller-rabin 7)       ; #t
(miller-rabin 1009)    ; #t
(miller-rabin 1000033) ; #t

;; Non-prime numbers
(miller-rabin 8)       ; #f
(miller-rabin 1015)    ; #f
(miller-rabin 1000034) ; #f

;; Carmichael numbers
(miller-rabin 561)     ; #f
(miller-rabin 1105)    ; #f
(miller-rabin 1729)    ; #f
(miller-rabin 2465)    ; #f
(miller-rabin 2821)    ; #f
(miller-rabin 2821)    ; #f
(miller-rabin 6601)    ; #f
