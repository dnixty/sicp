;; Recursive process
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

;; Iterative process
(define (f n)
  (define (f-iter a b c count)
    (cond ((< count 2) count)
          ((= count 2) c)
          (else (f-iter b
                        c
                        (+ c (* 2 b) (* 3 a))
                        (- count 1)))))
  (f-iter 0 1 2 n))
