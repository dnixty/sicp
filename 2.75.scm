(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos z)))
          ((eq? op 'imag-part) (* r (sin z)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op:
            MAKE-FROM-MAG-ANG" op)))))
