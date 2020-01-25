(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (identity x)
  x)

(define (inc x)
  (+ 1 x))

(define (factorial n)
  (product identity 1 inc n))

(define (pi n)
  (define (pi-next n)
    (+ 2 n))
  (* 4.0 (/ (* 2 (product square 4 pi-next (- n 1)))
            (product square 3 pi-next n))))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))
