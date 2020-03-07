(define random-init (random 100))

(define (rand-update x) (+ x 1))

(define rand
  (let ((x random-init))
    (define (dispatch m)
      (cond ((eq? m 'generate)
             (begin (set! x (rand-update x)) x))
            ((eq? m 'reset)
             (lambda (new-value) (set! x new-value) x))))
    dispatch))

(rand 'generate)
((rand 'reset) 0)
