(define (make-table)
  (let ((local-table (list '(*table*) '() '())))
    (define (entry tree) (car tree))
    (define (left-branch tree) (cadr tree))
    (define (right-branch tree) (caddr tree))
    (define (make-tree entry left right)
      (list entry left right))
    (define key car)
    (define (lookup given-key)
      (define (loop tree)
        (cond ((null? tree) #f)
            ((equal? given-key (key (entry tree)))
             (entry tree))
            ((symbol<? given-key (key (entry tree)))
             (loop (left-branch tree)))
            (else
             (loop (right-branch tree)))))
      (loop local-table))
    (define (insert! given-key value)
      (define (loop tree)
        (cond ((symbol<? given-key (key (entry tree)))
               (if (null? (left-branch tree))
                   (set-cdr! tree
                             (list (make-tree (cons given-key value)
                                              '() '())
                                   (right-branch tree)))
                   (loop (left-branch tree))))
              ((symbol>? given-key (key (entry tree)))
               (if (null? (right-branch tree))
                   (set-cdr! tree
                             (list (left-branch tree)
                                   (make-tree (cons given-key value)
                                              '() '())))
                   (loop (right-branch tree))))
              (else (set-car! tree (cons given-key value)))))
      (loop local-table)
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation:
                          TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 'dadada 7)
(put 'ahaha 5)
(put 'hohoho 9)
(get 'ahaha)
(get 'notfound)
