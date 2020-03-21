(define
  (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

;;; Louis is wrong as the transfer operation doesn't require both
;;; accounts to be indle before we start transfering. This operation
;;; can happen in any sequence.
