(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))


;; This procedure builds the tree recursively inside the car added to
;; the given list. On the cadr of the list there's always a current
;; entry. Left branch is calculated recursively by remembering all the
;; elemenets from the list until it can't be divided into other
;; branches anymore. Then it calculates right branch recursively by
;; removing just used entry for the left branch and doing excatly the same operation as with left branch. The actual tree is constructed on the way back from the lowest level of the tree to the top entry.

;; (list->tree '(1 3 5 7 9 11))
;;      5
;;     / \
;;    1   9
;;    \   /\
;;     3 7 11

;; The order of growth in the number of steps required by list->tree
;; is Θ(n)
