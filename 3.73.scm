(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
     (apply proc (map stream-car argstreams))
     (apply stream-map
            (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (add-streams (scale-stream integrand dt)
                  int)))
  int)

(define (RC R C dt)
  (lambda (i v0)
    (add-streams (scale-stream i R)
                 (integral (scale-stream i (/ 1 C))
                           v0
                           dt))))

;;; R = 5, C = 1, dt = 0.5s
(define RC1 (RC 5 1 0.5))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define (display-line x)
  (display x)
  (newline))
(define (show-stream s n)
  (if (zero? n)
      (display-line "done")
      (begin
        (display-line (stream-car s))
        (show-stream (stream-cdr s) (- n 1) ))))

(show-stream (RC1 integers 0.2) 5)
