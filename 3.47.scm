(define (make-semaphore n)
  (let ((lock (make-mutex))
        (taken 0))
    (define (semaphore command)
      (cond ((eq? command 'acquire)
             (lock 'acquire)
             (if (< taken n)
                 (begin (set! taken (1+ taken)) (lock 'release))
                 (begin (lock 'release) (semaphore 'acquire))))
            ((eq? command 'release)
             (lock 'acquire)
             (set! taken (1- taken))
             (lock 'release))))
    semaphore))
