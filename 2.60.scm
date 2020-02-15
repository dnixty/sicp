(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
;; Θ(n)

(define (adjoin-set x set)
  (cons x set))
;; Θ(1)

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))
;; Θ(n^2)

(define (union-set set1 set2)
  (append set1 set2))
;; Θ(n)

;; The only application for which this representation would be in
;; preference to the non-duplicate one is when the data creating speed
;; is more crucial than operations on that data

;; Testing
(intersection-set '(1 1 4 2 3 5 5) '(1 2 5 5 5 9 9))
