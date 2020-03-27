;;; ln 2 = 1 - 1/2 + 1/3 - 1/4 + ...
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

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream
     (- s2 (/ (square (- s2 s1))
              (+ s0 (* -2 s1) s2)))
     (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream
   s
   (make-tableau
    transform
    (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s) (partial-sums s))))

(define (ln2-summands n)
  (cons-stream
   (/ 1.0 n)
   (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

(show-stream ln2-stream 8)
(show-stream (euler-transform ln2-stream) 8)
(show-stream (accelerated-sequence euler-transform ln2-stream) 8)
