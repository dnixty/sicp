;; Iterative process
(define (accumulate combiner null-value term a next b)
  (define (iter a acc)
    (if (> a b)
        acc
        (iter (next a) (combiner acc (term a)))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

;; Recursive process
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (accumulate combiner null-value term (next a) next b)
                (term a))))
