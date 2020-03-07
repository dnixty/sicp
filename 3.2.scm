(define (make-monitored f)
  (define (mf counter)
    (define (dispatch m)
      (if (eq? m 'how-many-calls?)
          counter
          (begin (set! counter (+ counter 1))
                 (f m))))
    dispatch)
  (mf 0))

(define s (make-monitored sqrt))

(s 100)

(s 'how-many-calls?)
