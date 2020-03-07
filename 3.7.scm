(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (cond ((not (eq? p password)) (lambda (x) "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request:
                 MAKE-ACCOUNT" m))))
  dispatch)

(define (make-joint account orig-pass new-pass)
  (define (dispatch p m)
    (if (not (eq? p new-pass))
        (lambda (x) "Incorrect password")
        (account orig-pass m)))
  dispatch)

(define peter-acc
  (make-account 100 'open-sesame))

(define paul-acc
  (make-joint peter-acc 'open-sesam 'rosebud))

((peter-acc 'open-sesame 'withdraw) 20)
((paul-acc 'rosebud 'withdraw) 20)
((paul-acc 'rosebud 'deposit) 40)
