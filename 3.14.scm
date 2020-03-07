(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))


;; Mystery reverses the list x by destroying it contents
;; After the evaluation:
;; v (a)
;; w (d c b a)
