(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low
            (enumerate-interval
             (+ low 1)
             high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (queens board-size)
  (define empty-board '())
  (define (adjoin-position new-row k rest-of-queens)
    (define (adjoin-iter i res)
      (cond ((= i (+ board-size 1)) res)
            ((= i new-row) (adjoin-iter (+ i 1) (cons #t res)))
            (else (adjoin-iter (+ i 1) (cons #f res)))))
    (cons (adjoin-iter 1 '()) rest-of-queens))
  (define (safe? k positions)
    (define (iter first rest result)
      (if (null? result)
          result
          (iter first (cdr rest)
                (and (not (= first (car rest)))
                     result))))
    (iter (car positions) (cdr positions) #t))
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
