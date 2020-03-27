(define (list-of-values-l2r exps env)
  (if (no-operands? exps)
      '()
      (let ((first-exp (eval (first-operand exps) env)))
        (cons first-exp
              (list-of-values-l2r (rest-operands exps) env)))))

(define (list-of-values-r2l exps env)
  (list-of-values-l2r (reverse exps) env))
