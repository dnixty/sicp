(define (display-line x)
  (display x)
  (newline))

(define (show-stream s n)
  (if (zero? n)
      (display-line "done")
      (begin
        (display-line (stream-car s))
        (show-stream (stream-cdr s) (- n 1) ))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x)
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map
     (lambda (x) (append (list (stream-car s)) x))
     (stream-cdr (pairs t u)))
    (triples
     (stream-cdr s)
     (stream-cdr t)
     (stream-cdr u)))))

(define pythagorean-triples
  (stream-filter (lambda (t)
                   (= (+ (square (car t))
                         (square (cadr t)))
                      (square (caddr t))))
                 (triples integers integers integers)))

(show-stream pythagorean-triples 5)
