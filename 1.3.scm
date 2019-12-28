(define (answer a b c)
  (+ (if (> a b) (* a a) (* b b))
     (if (> b c) (* b b) (* c c))))
