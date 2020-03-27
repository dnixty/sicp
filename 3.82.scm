(define (add-streams s1 s2) (stream-map + s1 s2))
(define ones (cons-stream 1.0 ones))
(define integers (cons-stream 1.0 (add-streams ones integers)))

(define (random-stream lo hi)
  (define (random-in-range low high)
    (let ((range (- high low)))
      (+ low (random range))))
  (cons-stream (random-in-range lo hi) (random-stream lo hi)))

(define (estimate-integral p x1 x2 y1 y2)
  (define throw-results (stream-map (lambda (x) (if (eq? x true) 1.0 0))
                                    (stream-map p (random-stream x1 x2) (random-stream y1 y2))))
  (define succesful-throws
    (cons-stream (stream-car throw-results) (add-streams (stream-cdr throw-results) succesful-throws)))
  (define (get-area probability) (* probability (abs (* (- y2 y1) (- x2 x1)))))
  (stream-map get-area (stream-map / succesful-throws integers)))
