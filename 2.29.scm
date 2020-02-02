(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile) (car mobile))

(define (right-branch mobile) (car (cdr mobile)))

(define (branch-length branch) (car branch))

(define (branch-structure branch) (car (cdr branch)))

(define (total-weight m)
  (cond ((null? m) 0)
        ((not (pair? m)) m)
        (else (+ (total-weight (branch-structure (left-branch m)))
                 (total-weight (branch-structure (right-branch m)))))))

(define (balanced? m)
  (define (torque b)
    (* (branch-length b) (total-weight (branch-structure b))))
  (if (not (pair? m))
      #t
      (and (= (torque (left-branch m))
              (torque (right-branch m)))
           (balanced? (branch-structure (left-branch m)))
           (balanced? (branch-structure (right-branch m))))))

(define b1 (make-branch 4 1))
(define b2 (make-branch 4 1))
(define b3 (make-branch 2 1))
(define m1 (make-mobile b1 b2))
(define b4 (make-branch 1 m1))
(define m (make-mobile b3 b4))

(left-branch m)
(right-branch m)
(branch-length b1)
(branch-structure b4)
(total-weight m)
(balanced? m)

;;                m
;;              /   \
;;             /     \
;;          (2 1)   (1 m1)
;;                    / \
;;                   /   \
;;                 (4 1) (4 1)

;; If we change the representation of mobiles we just need to update
;; the definition of `left-branch', `right-branch', `branch-length',
;; and `branch-structure'.
