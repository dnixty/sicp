(define (display-line x)
  (display x)
  (newline))

(define (show-stream s n)
  (if (zero? n)
      (display-line "done")
      (begin
        (display-line (stream-car s))
        (show-stream (stream-cdr s) (- n 1) ))))

(define (merge-weighted s1 s2 w)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< (w s1car) (w s2car))
                  (cons-stream
                   s1car
                   (merge-weighted (stream-cdr s1) s2 w)))
                 ((> (w s1car) (w s2car))
                  (cons-stream
                   s2car
                   (merge-weighted s1 (stream-cdr s2) w)))
                 (else
                  (cons-stream
                   s1car
                   (merge-weighted (stream-cdr s1)
                                   (stream-cdr s2)
                                   w))))))))

(define (pairs s t w)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t) w)
    w)))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define a
  (pairs integers integers (lambda (x) (+ (car x) (cadr x)))))
(define b
  (stream-filter (lambda (t) (and (not (= (modulo (car t) 2) 0))
                                  (not (= (modulo (car t) 3) 0))
                                  (not (= (modulo (car t) 5) 0))
                                  (not (= (modulo (cadr t) 2) 0))
                                  (not (= (modulo (cadr t) 3) 0))
                                  (not (= (modulo (cadr t) 5) 0))))
                 (pairs integers
                        integers
                        (lambda (x) (+ (* 2 (car x))
                                       (* 3 (cadr x))
                                       (* 5 (car x) (cadr x)))))))
