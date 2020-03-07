(define f
  (let ((y 0))
    (lambda (x)
      (if (= x 1)
          (begin (set! y 1) 0)
          y))))

(+ (f 0) (f 1))
