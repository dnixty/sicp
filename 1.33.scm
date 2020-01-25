(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n)
  (if (= n 1)
      #f
      (fast-prime? n 1000)))

(define (inc n)
  (+ n 1))

(define (identity n)
  n)

(define (filtered-accumulate combiner null-value term a next b pred)
  (cond ((> a b) null-value)
        ((pred a) (combiner (filtered-accumulate combiner null-value term (next a) next b pred)
                            (term a)))
        (else (filtered-accumulate combiner null-value term (next a) next b pred))))


(define (f a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (g n)
  (define (relative-prime? i)
    (= (gcd i n) 1))
  (filtered-accumulate * 1 identity 1 inc n relative-prime?))
