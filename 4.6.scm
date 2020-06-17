(define (eval exp env)
  (cond ((self-evaluating? exp)
         exp
         ((variable? exp)
          (lookup-variable-value exp env))
         ((quoted? exp)
          (text-of-quotation exp))
         ((assignment? exp)
          (eval-assignment exp env))
         ((definition? exp)
          (eval-definition exp env))
         ((if? exp)
          (eval-if exp env))
         ((lambda? exp)
          (make-procedure
           (lambda-parameters exp)
           (lambda-body exp)
           env))
         ((let? exp)
          (eval (let->combination exp) env))
         ((begin? exp)
          (eval-sequence
           (begin-actions exp)
           env))
         ((cond? exp)
          (eval (cond->if exp) env))
         ((application? exp)
          (apply (eval (operator exp) env)
                 (list-of-values
                  (operands exp)
                  env)))
         (else
          (error "Unknwon expression type: EVAL" exp)))))


(define (let? exp) (tagged-list? exp 'let))
(define (let->combination exp)
  (expand-let (let-definitions exp) (let-body exp)))
(define (let-definitions exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (expand-let definitions body)
  (let ((vars (map car definitions))
        (exps (map cadr definitions)))
    (cons (make-lambda vars body) exps)))

;; Test
(let->combination '(let ((a 1) (b 2)) (+ a b)))