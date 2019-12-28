(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; If b is greater than zero then subtract b from a which effectively
;; mean add an absolute value of b to a. Otherwise, add (positive
;; value) of b to a.
;; For example:
;; (a-plus-abs-b 1 2)
;; ((if (> 2 0) + -) 1 2)
;; ((if #t + -) 1 2)
;; (+ 1 2)
;; 3
;;
;; (a-plus-abs-b 1 -2)
;; ((if (> -2 0) + -) 1 -2)
;; ((if #f + -) 1 -2)
;; (- 1 -2)
;; 3
