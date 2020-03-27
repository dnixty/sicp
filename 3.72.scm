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


(define (two-squares-numbers)
  (define (sum-squared x)
    (let ((i (car x)) (j (cadr x)))
      (+ (* i i) (* j j))))
  (define (two-squares all-sum-squares)
    (let* ((current (stream-car all-sum-squares))
           (next (stream-car (stream-cdr all-sum-squares)))
           (second-next (stream-car (stream-cdr (stream-cdr all-sum-squares))))
           (two-squares-candidate (sum-squared current)))
      (cond ((= two-squares-candidate
                (sum-squared next)
                (sum-squared second-next))
             (cons-stream (list two-squares-candidate current next second-next)
                          (two-squares (stream-cdr (stream-cdr (stream-cdr all-sum-squares))))))
            (else (two-squares (stream-cdr all-sum-squares))))))
  (two-squares (weighted-pairs integers
                               integers
                               sum-squared)))

(show-stream (two-squares-numbers) 6)
