(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2
                          (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2
                          (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr!
                   subtable
                   (cons (cons key-2 value)
                         (cdr subtable)))))
            (set-cdr!
             local-table
             (cons (list key-1
                         (cons key-2 value))
                   (cdr local-table)))))
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

(define tag car)
(define values cadr)

(define (get-record key records)
  ((get 'get-record (tag records))
   key
   (values records)))

(define (get-salary key records)
  ((get 'get-salary (tag records))
   key
   (values records)))

(define (find-employee-record key all-records)
  (if (null? all-records)
      false
      (or (get-record key (car all-records))
          (find-employee-record key (cdr all-records)))))

(define (install-sales-package)
  (define get-key car)
  (define (get-record key records)
    (cond ((null? records) false)
          ((equal? key (get-key (car records)))
           (car records))
          (else (get-record key (cdr records)))))
  (define (get-salary key records)
    (let ((record (get-record key records)))
      (if record (caddr record) false)))
  (put 'get-record 'sales get-record)
  (put 'get-salary 'sales get-salary))

(define (install-management-package)
  (define get-key car)
  (define (get-record key node)
    (cond ((null? node) false)
          ((equal? key (get-key node)) (cons (car node) (cadr node)))
          ((symbol<? key (get-key node))
           (get-record key (caddr node)))
          (else (get-record key (cadddr node)))))
  (define (get-salary key node)
    (let ((record (get-record key node)))
      (if record (cadr record) false)))
  (put 'get-record 'management get-record)
  (put 'get-salary 'management get-salary))

(define sales-records '(sales ((arnhein address1 30000)
                               (bowler address2 25000)
                               (dick address10 22000)
                               (king address12 29000)
                               (teddy address11 32000)
                               (smith address13 33000))))

(define management-records
  '(management (kane (80000 address3)
                     (bolton (70000 address4)
                             (adams (75000 address5)
                                    ()
                                    (balmer (68000 address14) () ()))
                             (dalton (70000 address6)
                                     ()
                                     (euler (100000 address7)
                                            ()
                                            ())))
                     (rocker (50000 address8)
                             (paulsen (55000 address9)
                                      ()
                                      ())
                             (steward (47000 adress15)
                                      ()
                                      ())))))

(define all-records (list sales-records management-records))

(install-sales-package)
(install-management-package)

;; To incorporate the new personel information we just have to add new
;; package which defines get-record and get-salary procedures and
;; installs them into the procedure table under a specified tag. The next thing is to assign the tag the database by consing a tag into the database list.
