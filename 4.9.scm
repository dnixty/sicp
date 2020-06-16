(define (make-begin seq) (cons 'begin seq))

(define (while? exp) (tagged-list? exp 'while))
(define (while-clauses exp) (cdr exp))
(define (while-pred clauses) (cadr clauses))
(define (while-body clauses) (caddr clauses))
(define (while->loop exp)
  (while-expand-clauses (while-clauses exp)))
(define (while-expand-clauses clauses)
  (make-begin
   (list
    (list 'define (cons 'while-aux '())
          (list 'cond
                (list (list 'not (list (while-pred clauses))) ''done)
                (append (cons 'else (while-body clauses)) (list (list 'while-aux))))))))

;;; Testing
(while-expand-clauses '(while (< i 100) ((display i) (set! i (+ i 1)))))
