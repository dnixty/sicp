;; Assume `double' and `halve' are defined by the language
(define (double a)
  (+ a a))
(define (halve a)
  (/ a 2))

(define (* a b)
  (define (iter acc a b)
    (cond ((= b 0) acc)
          ((even? b) (iter acc (double a) (halve b)))
          (else (iter (+ acc a) a (- b 1)))))
  (iter 0 a b))

;; Testing
(* 2 3)
(* 3 3)
(* 4 0)
(* 0 4)
(* 4 4)
(* 5 4)
