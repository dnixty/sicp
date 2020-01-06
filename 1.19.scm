;; Tpq transforms the pair (a, b) according to:
;; a'  <- bq + aq + ap
;; b'  <- bp + aq
;;
;; Applying this transformation twice we have
;; a'' <- (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
;;        bpq + aq^2 + bq^2 + aq^2 + apq + bpq + apq + ap^2
;;        2bpq + bq^2 + 2apq + aq^2 + aq^2 + ap^2
;;        b(2pq + q^2) + a(2pq + q^2) + a(p^2 + q^2)
;; b'' <- (bp + aq)p + (bq + aq + ap)q
;;        bp^2 + apq + bq^2 + aq^2 + apq
;;        b(p^2 + q^2) + a(2pq + q^2)
;;
;; Let q' = 2pq + q^2
;;     p' = p^2 + q^2
;;
;; Now using new pair (a'', b'')  we can write down the new
;; transformation Tp'q' when Tpq is applied twice as:
;; a'' <- bq' + aq' + ap
;; b'' <- bp' + aq'
;;
;; Thus we can see that the effect of applying Tpq twice is the same
;; as applying single transformation Tp'q'.

;; Fibonacci
(define (square n)
  (* n n))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 (* p q)) (square q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;; Testing
(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)
(fib 8)
(fib 9)
