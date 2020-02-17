(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var)
             1
             0))
        (else ((get 'deriv (operator exp))
               (operands exp)
               var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; This function gets a deriv procedure from a table of operations
;; based on a type of operation. The type is being determined by car
;; of an expression. We can determine the deriv procedure for numbers
;; and variables because they don't have operators.

(define (install-deriv-package)
  (define (deriv-sum args var)
    (make-sum (deriv (car args) var)
              (deriv (cdr args) var)))

  (define (deriv-product args var)
    (make-sum
     (make-product
      (car args)
      (deriv (cadr args) var))
     (make-product
      (deriv (car args) var)
      (cadr args))))

  (define (deriv-exponentiation args var)
    (make-product
     (make-product
      (cadr args)
      (make-exponentiation
       (car args)
       (make-sum (cadr args) -1)))))

  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** deriv-exponentiation))

;; If we indexed the procedures in the opposite way we would have to
;; change auxiliary code required to install procedures in the
;; table. For example (put 'deriv '+ sum) would be in a form (put '+
;; 'deriv sum).
