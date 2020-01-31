(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))

;; This implementation of `square-list' is returning the list in the
;; reverse order because the `iter' procedure builds the answer
;; starting from the bottom of the list.

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))

(square-list (list 1 2 3 4))

;; This implementation doesn't work as well because the list data
;; structure is asymmetrical.
