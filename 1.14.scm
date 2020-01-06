(define (count-change amount)
  (cc amount 5))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; (count-change 11)
;; |
;; (cc 11 5)__
;; |          \
;; (cc 11 4)   (cc -39 5)
;; |       \___
;; |           \
;; (cc 11 3)   (cc -14 4)
;; |       \_______________________________________________________
;; |                                                               \
;; (cc 11 2)                                                      (cc 1 3)
;; |       \_________________________                              |     \__
;; |                                 \                             |        \
;; (cc 11 1)                        (cc 6 2)                      (cc 1 2) (cc -9 3)
;; |       \___                      |     \__                     |     \__
;; |           \                     |        \                    |        \
;; (cc 11 0)   (cc 10 1)            (cc 6 1) (cc 1 2)             (cc 1 1) (cc -4 2)
;;          __/ |                 __/ |       |     \__            |     \__
;;         /    |                /    |       |        \           |        \
;; (cc 10 0)   (cc 9 1)  (cc 6 0)   (cc 5 1) (cc 1 1) (cc -4 2)   (cc 1 0) (cc 0 1)
;;          __/ |                 __/ |       |     \__
;;         /    |                /    |       |        \
;; (cc 9 0)    (cc 8 1)  (cc 5 0)   (cc 4 1) (cc 1 0) (cc 0 1)
;;          __/ |                 __/ |
;;         /    |                /    |
;; (cc 8 0)    (cc 7 1)  (cc 4 0)   (cc 3 1)
;;          __/ |                 __/ |
;;         /    |                /    |
;; (cc 7 0)    (cc 6 1)  (cc 3 0)   (cc 2 1)
;;          __/ |                 __/ |
;;         /    |                /    |
;; (cc 6 0)    (cc 5 1)  (cc 2 0)   (cc 1 1)
;;          __/ |                 __/ |
;;         /    |                /    |
;; (cc 5 0)    (cc 4 1)  (cc 1 0)   (cc 0 1)
;;          __/ |
;;         /    |
;; (cc 4 0)    (cc 3 1)
;;          __/ |
;;         /    |
;; (cc 3 0)    (cc 2 1)
;;          __/ |
;;         /    |
;; (cc 2 0)    (cc 1 1)
;;          __/ |
;;         /    |
;; (cc 1 0)    (cc 0 1)
;;
;; Space complexity:
;; The order of growth of the space is proportional to the maximum
;; depth of the tree, which is denominated by the maximum depth of
;; the subtree where the amount is decreased by 1. Clearly that is:
;; Θ(n)
;;
;; Time complexity:
;; The order of growth of the steps used by this process is going
;; to be dominated by the number of kinds-of-coins (for relatively
;; large n). For 5 different denomination the tree will spawn 5
;; different subtrees which indicates that the order of growth is:
;; Θ(n^5)
