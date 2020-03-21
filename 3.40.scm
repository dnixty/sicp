(define x 10)
(parallel-execute
 (lambda () (set! x (* x x)))
 (lambda () (set! x (* x x x))))

;;; [100, 1000, 10000, 100000, 1000000]

(define x 10)
(define x (make-serializer))
(parallel-execute
 (s (lambda () (set! x (* x x))))
 (s (lambda () (set! x (* x x x)))))

;;; Only 1000000
