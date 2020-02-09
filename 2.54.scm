(define (equal? x y)
  (cond ((and (null? x) (null? y)) #t)
        ((or (null? x) (null? y)) #f)
        (else (and (= (car x)
                      (car y))
                   (equal? (cdr x)
                           (cdr y))))))

;; Testing
(equal? '(1 2 3) '(1 2 3))
(equal? '(1 2 3) '(1 2))
(equal? '(1 2) '(1 2 3))
