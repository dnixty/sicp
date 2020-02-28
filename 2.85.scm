(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2
                          (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2
                          (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr!
                   subtable
                   (cons (cons key-2 value)
                         (cdr subtable)))))
            (set-cdr!
             local-table
             (cons (list key-1
                         (cons key-2 value))
                   (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation:
                          TABLE" m))))
    dispatch))
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'raise 'scheme-number
       (lambda (x) (make-rational x 1)))
  (put 'project '(scheme-number)
       (lambda (x) (make-scheme-number x)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (eq? x y)))
  'done)

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (equ-rat? x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x))))
  (define (tag x) (attach-tag 'rational x))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'raise 'rational
       (lambda (x) (make-real (/ (numer x) (denom x)))))
  (put 'project '(rational)
       (lambda (x)
         (make-scheme-number (car
                              (integer-divide (numer x)
                                              (denom x))))))
  (put 'equ? '(rational rational)
       (lambda (x y) (equ-rat? x y)))
  'done)

(define (install-real-package)
  (define (make-real x) x)
  (define (tag x) (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (+ x y)))
  (put 'make 'real
       (lambda (x) (tag (make-real x))))
  (put 'raise 'real
       (lambda (x) (make-complex-from-real-imag x 0)))
  (put 'project '(real)
       (lambda (x)
         (make-rational (round x) 1)))
  (put 'equ? '(real real)
       (lambda (x y) (eq? x y)))
  'done)

(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  (define (tag x)
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  (put 'project '(rectangular)
       (lambda (x)
         (make-real (real-part x))))
  (put 'equ? '(rectangular rectangular)
       (lambda (x y) (and (= (real-part x) (real-part y))
                          (= (imag-part x) (imag-part y)))))
  'done)

(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-mag-ang r a) (cons r a))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  (put 'project '(polar)
       (lambda (x)
         (make-real (real-part x))))
  (put 'equ? '(polar polar)
       (lambda (x y) (and (= (magnitude x) (magnitude y))
                          (= (angle x) (angle y)))))
  'done)

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag
          'rectangular)
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar)
     r a))
  (define (tag z) (attach-tag 'complex z))
  (put 'make-from-real-imag 'complex
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  (put 'project '(complex)
       (lambda (x)
         (apply-generic 'project x)))
  (put 'equ? '(complex complex) equ?)
  'done)

(install-scheme-number-package)
(install-rational-package)
(install-real-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum:
              TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum:
              CONTENTS" datum)))

(define (raise x)
  (let ((proc (get 'raise (type-tag x))))
    (if proc
        (proc (contents x))
        x)))

(define (raise-into x y)
  (let ((raised-x (raise x)))
    (cond ((equal? (type-tag x) (type-tag y)) x)
          ((equal? (type-tag x) (type-tag raised-x)) #f)
          (else (raise-into (raise x) y)))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let* ((a1 (car args))
                    (a2 (cadr args))
                    (raised-a1 (raise-into a1 a2))
                    (raised-a2 (raise-into a2 a1)))
                (cond (raised-a1 (apply-generic op raised-a1 a2))
                      (raised-a2 (apply-generic op a1 raised-a2))
                      (else (error "No method for these types"
                                   (list
                                    op
                                    type-tags)))))
              (error
               "No method for these types"
               (list op type-tags)))))))
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (make-real x)
  ((get 'make 'real) x))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
(define (add x y)
  (apply-generic 'add x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (project x)
  (apply-generic 'project x))
(define (drop x)
  (let ((projected (project x)))
    (cond ((equal? projected x) x)
          ((equ? (raise projected) x)
           (drop projected))
          (else x))))

;; Testing
(define complex (make-complex-from-real-imag 3 1))
(define complex2 (make-complex-from-real-imag 3 0))
(drop complex)
(drop complex2)
