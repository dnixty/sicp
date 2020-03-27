(define (display-line x)
  (display x)
  (newline))

(define (show-stream s n)
  (if (zero? n)
      (display-line "done")
      (begin
        (display-line (stream-car s))
        (show-stream (stream-cdr s) (- n 1) ))))

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

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s1)
                                          (stream-cdr s2))
                            (mul-series s1
                                        (stream-cdr s2)))))

(define (invert-unit-series s)
  (define memoized
    (cons-stream 1 (scale-stream
                  (mul-series (stream-cdr s)
                              memoized)
                  -1)))
  memoized)

(define (div-series s1 s2)
  (mul-series s1 (invert-unit-series s2)))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define (integrate-series s)
  (multiply-streams s (divide-streams ones integers)))
(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(div-series sine-series cosine-series)
