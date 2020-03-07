(define (count-pairs x)
  (let ((encountered '()))
    (define (helper x)
      (if (or (not (pair? x)) (memq x encountered))
          0
          (begin
            (set! encountered (cons x encountered))
            (+ (helper (car x))
               (helper (cdr x))
               1))))
    (helper x)))

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
(count-pairs x)
