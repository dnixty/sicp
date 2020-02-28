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
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (eq? x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (eq? x 0)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'minus '(scheme-number)
       (lambda (x) (tag (- x))))
  (put 'raise 'scheme-number
       (lambda (x) (make-rational x 1)))
  'done)

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  ;; procedures same-variable? and variable? from section 2.3.2
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (normalize p v)
    (make-poly v (adjoin-term
                  (make-term 0 (tag p))
                  (the-empty-termlist))))

  ;; procedures used by add-poly
  (define (add-poly p1 p2)
    (cond ((variable<? (variable p1) (variable p2))
           (add-poly p1 (normalize p2 (variable p1))))
          ((variable>? (variable p1) (variable p2))
           (add-poly p2 p1))
          (else (make-poly (variable p1)
                           (add-terms (term-list p1)
                                      (term-list p2))))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  ;; procedures used by mul-poly
  (define (mul-poly p1 p2)
    (cond ((variable<? (variable p1) (variable p2))
           (mul-poly p1 (normalize p2 (variable p1))))
          ((variable>? (variable p1) (variable p2))
           (mul-poly p2 p1))
          (else (make-poly (variable p1)
                           (mul-terms (term-list p1)
                                      (term-list p2))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (=zero-all-terms? L)
    (or (empty-termlist? L)
        (and (=zero? (coeff (first-term L)))
             (=zero-all-terms? (rest-terms L)))))

  (define (minus-all-terms L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((current-term (first-term L)))
          (cons (make-term (order current-term)
                           (minus (coeff current-term)))
                (minus-all-terms (rest-terms L))))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial)
       (lambda (p)
         (=zero-all-terms? (term-list p))))
  (put 'minus '(polynomial)
       (lambda (p)
         (tag (make-poly (variable p)
                         (minus-all-terms (term-list p))))))
  'done)

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (tag x) (attach-tag 'rational x))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'raise 'rational
       (lambda (x) (make-real (/ (numer x) (denom x)))))
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
  'done)

(define (install-rectangular-package)
  (define (make-from-real-imag x y)
    (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  (define (tag x)
    (attach-tag 'rectangular x))
  (put 'make-from-real-imag 'rectangular
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  (define (make-from-mag-ang r a) (cons r a))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  (define (tag x) (attach-tag 'polar x))
  (put 'make-from-real-imag 'polar
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
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
  (put 'raise 'complex
       (lambda (x) (make-polynomial 'fallback-var
                                    (list (0 x)))))
  'done)

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

(install-scheme-number-package)
(install-rational-package)
(install-real-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-polynomial-package)

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
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
(define (=zero? x)
  (apply-generic '=zero? x))
(define (add x y)
  (apply-generic 'add x y))
(define (minus x)
  (apply-generic 'minus x))
(define (sub x y)
  (apply-generic 'add x (minus y)))

(define (variable<? v1 v2)
  (string<? (symbol->string v1) (symbol->string v2)))

(define (variable>? v1 v2)
  (variable<? v2 v1))

(define n1 (make-scheme-number 1))
(define n2 (make-scheme-number 2))
(define n3 (make-scheme-number 3))

(define p1 (make-polynomial 'x (list (list 1 n2) (list 0 n1))))
(define p2 (make-polynomial 'y (list (list 1 n3))))
(define p3 (make-polynomial 'z (list (list 2
                                           (make-polynomial 'y (list (list 1 n1) (list 0 n1))))
                                     (list 1
                                           (make-polynomial 'y (list (list 2 n1)))))))

;; 2x
;; 3y

;; 2x + 3y
