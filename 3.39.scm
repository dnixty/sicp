(define x 10)
(define s (make-serializer))
(parallel-execute
 (lambda ()
   (set! x ((s (lambda () (* x x))))))
 (s (lambda () (set! x (+ x 1)))))

;;; 101: P 1  sets x to 100 and then P 2  increments
;;;      x to 101.
;;; 121: P 2  increments x to 11 and then P 1  sets
;;;      x to x times x.
;;; 100: P 1  accesses x (twice), then P 2  sets
;;;      x to 11, then P 1  sets x.
