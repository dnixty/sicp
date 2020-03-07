(define (make-withdraw initial-amount)
  ((lambda (balance)
     (lambda (amount)
       (if (>= balance amount)
           (begin (set! balance
                        (- balance amount))
                  balance)
           "Insufficient funds")))
   initial-amount))

;;; GLOBAL
;;; make-withdraw:
;;;   parameter: initial-amount
;;;   body: ((lambda (balance) (...)))

(define W1 (make-withdraw 100))

;;; GLOBAL
;;; make-withdraw:
;;;   parameter: initial-amount
;;;   body: ((lambda (balance) ...))
;;; W1:
;;;   parameter: amount
;;;   body: (if (>= balance amount) ...)

;;; E1->GLOBAL
;;; initial-amount:100
;;; ((lambda (balance) (...)) initial-amount)

;;; E2->E1
;;; balance: 100
;;; (lambda (amount) (...))

;;; E3->E2
;;; amount
;;; (if (>= balance amount) ...)

(W1 50)

;;; E2->E1
;;; balance: 50
;;; (lambda (amount) (...))

(define W2 (make-withdraw 100))

;;; GLOBAL
;;; make-withdraw:
;;;   parameter: initial-amount
;;;   body: ((lambda (balance) ...))
;;; W1:
;;;   parameter: amount
;;;   body: (if (>= balance amount) ...)
;;; W2:
;;;   parameter: amount
;;;   body: (if (>= balance amount) ...)

;;; E1->GLOBAL
;;; initial-amount:100
;;; ((lambda (balance) (...)) initial-amount)

;;; E2->E1
;;; balance: 50
;;; (lambda (amount) (...))

;;; E3->E2
;;; amount
;;; (if (>= balance amount) ...)

;;; E4->GLOBAL
;;; initial-amount:100
;;; ((lambda (balance) (...)) initial-amount)

;;; E5->E4
;;; balance: 100
;;; (lambda (amount) (...))

;;; E3->E4
;;; amount
;;; (if (>= balance amount) ...)
