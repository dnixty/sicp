(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

;;; z is an circular list of (a b c a b c ...)  If we try to compute
;;; (last-pair z) we end up with infinite loop
