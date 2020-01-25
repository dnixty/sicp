(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d))
        (s (if (< (* n d) 0)
               -1
               1)))
    (cons (* s (abs (/ n g))) (abs (/ d g)))))

(make-rat -1 3)
(make-rat 1 -3)
(make-rat -1 -3)
