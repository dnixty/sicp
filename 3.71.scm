(define (display-line x)
  (display x)
  (newline))

(define (show-stream s n)
  (if (zero? n)
      (display-line "done")
      (begin
        (display-line (stream-car s))
        (show-stream (stream-cdr s) (- n 1) ))))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (if (<= (weight s1car) (weight s2car))
               (cons-stream s1car
                            (merge-weighted (stream-cdr s1)
                                            s2
                                            weight))
               (cons-stream s2car
                            (merge-weighted s1
                                            (stream-cdr s2)
                                            weight)))))))
(define (weighted-pairs s t w)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) w)
    w)))

(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))


(define (ramanujan-numbers)
  (define (sum-cubed x)
    (let ((i (car x)) (j (cadr x)))
      (+ (* i i i) (* j j j))))
  (define (ramanujans all-sum-cubes)
    (let* ((current (stream-car all-sum-cubes))
           (next (stream-car (stream-cdr all-sum-cubes)))
           (ramanujan-candidate (sum-cubed current)))
      (cond ((= ramanujan-candidate
                (sum-cubed next))
             (cons-stream (list ramanujan-candidate current next)
                          (ramanujans (stream-cdr (stream-cdr all-sum-cubes)))))
            (else (ramanujans (stream-cdr all-sum-cubes))))))
  (ramanujans (weighted-pairs integers
                              integers
                              sum-cubed)))

(show-stream (ramanujan-numbers) 6)
