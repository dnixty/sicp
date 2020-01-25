(define (square n)
  (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test-all n)
  (define (try-it a)
    (if (= a n)
        #t
        (and (= (expmod a n n) a)
             (try-it (+ a 1)))))
  (try-it 1))

;; Prime numbers
(fermat-test-all 7)       ; #t
(fermat-test-all 1009)    ; #t
(fermat-test-all 1000033) ; #t

;; Non-prime numbers
(fermat-test-all 8)       ; #f
(fermat-test-all 1015)    ; #f
(fermat-test-all 1000034) ; #f

;; Carmichael numbers
(fermat-test-all 561)     ; #t
(fermat-test-all 1105)    ; #t
(fermat-test-all 1729)    ; #t
(fermat-test-all 2465)    ; #t
(fermat-test-all 2821)    ; #t
(fermat-test-all 2821)    ; #t
(fermat-test-all 6601)    ; #t
