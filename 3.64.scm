(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
     (apply proc (map stream-car argstreams))
     (apply stream-map
            (cons proc (map stream-cdr argstreams))))))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0 (stream-map
          (lambda (guess)
            (sqrt-improve guess x))
          guesses)))
  guesses)

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2.0))

(define (stream-limit s t)
  (if (< (abs (- (stream-car (stream-cdr s)) (stream-car s))) t)
      (stream-car s)
      (stream-limit (stream-cdr s) t)))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
