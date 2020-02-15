(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left) (weight right))))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set
         (make-leaf (car pair)
                    (cadr pair))
         (make-leaf-set (cdr pairs))))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch
                (car bits)
                current-branch)))
          (if (leaf? next-branch)
              (cons
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits)
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit:
               CHOOSE-BRANCH" bit))))


(define (encode message tree)
  (if (null? message)
      '()
      (append
       (encode-symbol (car message)
                      tree)
       (encode (cdr message) tree))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (encode-symbol symbol tree)
  (define (search current-branch result)
    (cond ((and (leaf? current-branch)
                (eq? symbol (symbol-leaf current-branch)))
           result)
          ((element-of-set? symbol (symbols (left-branch current-branch)))
           (search (left-branch current-branch) (append result '(0))))
          ((element-of-set? symbol (symbols (right-branch current-branch)))
           (search (right-branch current-branch) (append result '(1))))
          (else (error "Symbol doesn't exist in the tree"))))
  (search tree '()))

(define (successive-merge leaf-set)
  (if (eq? (length leaf-set) 1)
      (car leaf-set)
      (successive-merge
       (adjoin-set
        (make-code-tree (car leaf-set)
                        (cadr leaf-set))
        (cddr leaf-set)))))

(define (generate-huffman-tree pairs)
  (successive-merge
   (make-leaf-set pairs)))

(define pairs '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))

(define verse '(Get a job Sha na na na na na na na na))

(define chorus '(Wah yip yip yip yip yip yip yip yip yip Sha boom))

(define song (append verse verse chorus))

(define huffman-tree (generate-huffman-tree pairs))
(length (encode song huffman-tree))
;; 84 bits are neede to encode this message.

(* (length song) 3)
;; 108 bits are needed to encode this message in a fixed-length code
;; for the eight-symbol alphabet
