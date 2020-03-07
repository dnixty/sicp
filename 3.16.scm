(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define l (list 1 2 3))
(count-pairs l)

(define first (cons 1 2))
(define second (cons 1 2))
(define third (cons first second))
(set-car! second first)
(count-pairs third)

(define first (cons 1 2))
(define second (cons first first))
(define third (cons second second))
(count-pairs third)

(define x (list 1 2 3))
(set-cdr! x x)
;(count-pairs zinf)
