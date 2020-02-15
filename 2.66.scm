(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (lookup given-key tree)
  (cond ((null? tree) #f)
        ((equal? given-key (key (entry tree)))
         (entry tree))
        ((< given-key (key (entry tree)))
         (lookup given-key (left-branch tree)))
        (else
         (lookup given-key (right-branch tree)))))

(define key car)

(define t1 '((7 'a) ((3 'b) ((1 'c) () ()) ((5 'd) () ())) ((9 'e) () ((11 'f) () ()))))

(lookup 3 t1)
