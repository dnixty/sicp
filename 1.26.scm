(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;; Defining `expmod' like that transforms the Θ(log n) process into a
;; Θ(n) process. With (even? exp) we're removing half of the number
;; of steps in each recursive call. Unfortunately we're adding
;; the same amount of steps back with calculating
;; (expmod base (/ exp 2) m)
;; twice. This is because Lisp has applicative-order of evaluation.
