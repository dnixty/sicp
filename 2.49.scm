(define (make-vector x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vector (+ (xcor-vect v1)
                  (xcor-vect v2))
               (+ (ycor-vect v1)
                  (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vector (- (xcor-vect v1)
                  (xcor-vect v2))
               (- (ycor-vect v1)
                  (ycor-vect v2))))

(define (scale-vect v n)
  (make-vector (* n (xcor-vect v))
               (* n (ycor-vect v))))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (cddr frame))

(define make-segment cons)

(define start-segment car)

(define end-segment cdr)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect
      (scale-vect (xcor-vect v)
                  (edge1-frame frame))
      (scale-vect (ycor-vect v)
                  (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))

(define outline
  (let ((segments '((make-segment (make-vect 0 0) (make-vect 0 1))
                    (make-segment (make-vect 0 1) (make-vect 1 1))
                    (make-segment (make-vect 1 1) (make-vect 1 0))
                    (make-segment (make-vect 1 0) (make-vect 0 0)))))
    (segments->painter segments)))

(define cross
  (let ((segments '((make-segment (make-vect 0 0) (make-vect 1 1))
                    (make-segment (make-vect 0 1) (make-vect 1 0)))))
    (segments->painter segments)))

(define diamond
  (let ((segments '((make-segment (make-vect 0 0.5) (make-vect 0.5 1))
                    (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
                    (make-segment (make-vect 1 0.5) (make-vect 0.5 0))
                    (make-segment (make-vect 0.5 0) (make-vect 0 0.5)))))
    (segments->painter segments)))

(define wave
  (let ((segments '((make-segment (make-vect .25 0) (make-vect .35 .5))
                    (make-segment (make-vect .35 .5) (make-vect .3 .6))
                    (make-segment (make-vect .3 .6) (make-vect .15 .4))
                    (make-segment (make-vect .15 .4) (make-vect 0 .65))
                    (make-segment (make-vect 0 .65) (make-vect 0 .85))
                    (make-segment (make-vect 0 .85) (make-vect .15 .6))
                    (make-segment (make-vect .15 .6) (make-vect .3 .65))
                    (make-segment (make-vect .3 .65) (make-vect .4 .65))
                    (make-segment (make-vect .4 .65) (make-vect .35 .85))
                    (make-segment (make-vect .35 .85) (make-vect .4 1))
                    (make-segment (make-vect .4 1) (make-vect .6 1))
                    (make-segment (make-vect .6 1) (make-vect .65 .85))
                    (make-segment (make-vect .65 .85) (make-vect .6 .65))
                    (make-segment (make-vect .6 .65) (make-vect .75 .65))
                    (make-segment (make-vect .75 .65) (make-vect 1 .35))
                    (make-segment (make-vect 1 .35) (make-vect 1 .15))
                    (make-segment (make-vect 1 .15) (make-vect .6 .45))
                    (make-segment (make-vect .6 .45) (make-vect .75 0))
                    (make-segment (make-vect .75 0) (make-vect .6 0))
                    (make-segment (make-vect .6 0) (make-vect .5 .3))
                    (make-segment (make-vect .5 .3) (make-vect .4 0))
                    (make-segment (make-vect .4 0) (make-vect .25 0)))))
    (segments->painter segments)))
