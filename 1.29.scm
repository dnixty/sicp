(define (cube x)
  (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson f a b n)
  (let ((h (/ (- b a) n)))
    (define (yk k) (f (+ a (* h k))))
    (define (term k)
      (let ((y (f (+ a (* h k)))))
        (* (cond ((or (= k 0) (= k n)) 1)
                 ((even? k) 2)
                 (else 4))
           y)))
    (define (next n) (+ n 1))
    (* (/ h 3) (sum term 0 next n))))

(integral cube 0 1 0.01)  ; .24998750000000042
(integral cube 0 1 0.001) ; .249999875000001
(simpson cube 0 1 100)    ; 1/4
(simpson cube 0 1 1000)   ; 1/4
