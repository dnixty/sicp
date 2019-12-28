(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))

;; With an interpreter that uses applicative-order evaluation this
;; expression will produce an infinite loop because the arguments to
;; the function will be evaluated first. The second argument of the
;; function is (p) which evaluate to itself producing an infinite
;; loop.
;; (test 0 (p))
;; (test 0 (p))
;; (test 0 (p))
;; ...
;;
;; On the other hand, with an interpreter that uses normal-order
;; evaluation, this expression will evaluate to 0 as the second
;; argument (p) is never evaluated.
;; (test 0 (p))
;; (if (= 0 0) 0 (p))
;; (if #t 0 (p))
;; 0
