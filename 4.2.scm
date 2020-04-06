;;; (define x 3) tests true for pair? Thus it'll be identified by eval
;;; as application. Thus it'll try to apply define to x and 3 as
;;; arguments.

(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
