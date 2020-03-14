;;; 100 / 2 + 10 - 20
;;; 40

;;; (100 + 10) / 2 - 20
;;; 35

;;; (100 - 20) / 2 + 10
;;; 50

;;; (100 - 20 + 10) / 2
;;; 45


;;; Other possible values: 60
;;; Access balance: $100
;;;                             Access balance: $100
;;; new value: 100 - 20 = 80
;;;                             new value: 100 / 2 = 50
;;; set! balance to $80
;;;                             set! balance to 50
;;; 50 + 10
;;; 60

;;; 90
;;; Access balance: $100
;;;                             Access balance: $100
;;; new value: 100 / 2 = 50
;;;                             new value: 100 - 20
;;; set! balance to $50
;;;                             set! balance to 80
;;; 80 + 10
;;; 90
