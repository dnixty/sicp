(define (average x y) (/ (+ x y) 2))

(define (smooth input-stream)
  (stream-map average input-stream (stream-cdr input-stream)))

(define (make-zero-crossing input-stream)
  (stream-map
   sign-change-detector
   (stream-cdr input-stream)
   input-stream))

(define zero-crossings (make-zero-crossing (smooth sense-data average)))
