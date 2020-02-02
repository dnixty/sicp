(define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items)) (list (car items)))))

(define (deep-reverse items)
  (cond ((null? items) '())
        ((not (pair? (car items))) (reverse items))
        (else (append (deep-reverse (cdr items))
                      (list (deep-reverse (car items)))))))

(define x (list (list 1 2) (list 3 4)))

x
;; ((1 2) (3 4))

(reverse x)
;; ((3 4) (1 2))

(deep-reverse x)
;; ((4 3) (2 1))
