;; This works because there's no magnitude operation defined in the
;; complex package. If we assume that magnitude is defined by: (define
;; (magnitude z) (apply-generic 'magnitude z)) then we can see that
;; after adding (put 'magnitude '(complex) magnitude) and calling
;; magnitude on z, the 'complex tag will be stripped off from z and
;; we'll end up calling magnitude again but now as (magnitue
;; '(rectangular 3 4)) which is pointing to the rectangular package
;; where magnitude is defined and the result will be 5. Thus we can
;; see that apply-generic will be invoked 2 times.
