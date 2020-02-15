(define (adjoin-set x set)
  (cond ((or (null? set) (< x (car set))) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set) (adjoin-set x (cdr set))))))

;; Testing
(adjoin-set 4 '(1 2 3))
(adjoin-set 1 '(2 3 4))
(adjoin-set 2 '(1 3 4))
(adjoin-set 1 '())
