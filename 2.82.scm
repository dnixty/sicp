(define (apply-generic op . args)
  (define (type-tags args) (map type-tags args))

  (define (coerce args target)
    (map (lambda (arg)
           (let ((coercor (get-coercion (type-tag arg)
                                        (type-tag target))))
             (if coercor
                 (coercor target)
                 target)))
         args))

  (define (iterate next)
    (if (null? next)
        (error "No coercion rules available")
        (let ((new-args (coerce args (car next))))
          (let ((proc (get op (type-tags new-args))))
              (if proc
                  (apply proc (map contents new-args))
                  (iterate (cdr next)))))))

  (let ((proc (get op (type-tags args))))
    (if proc
        (apply proc (map contents args))
        (iterate args))))

;; Situation where this is not sufficiently general:
;; types: A B C
;; registered op: (op some-A some-B some-B)
;; registered coercion: A->B C->B
;; Situation: Evaluating (apply-generic op A B C) will only try (op A
;; B C), (op B B B) and fail while we can just coerce C to B to
;; evaluate (op A B B) instead
