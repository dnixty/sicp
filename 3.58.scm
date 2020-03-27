(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den)
           den
           radix)))

;;; Expand is fraction expansion

(stream-cdr (stream-cdr (stream-cdr (stream-cdr (expand 1 7 10)))))
;;; 1 4 2 8 5 1 4 2 8 5 ...
(stream-cdr (stream-cdr (stream-cdr (stream-cdr (expand 3 8 10)))))
;;; 3 7 5 0 0 0 0 0 0 0 ...
