;;; Since fibs is memoized recalling each previous term requires no
;;; additions. To compute the nth term n-1 additions are made. If we
;;; would implement (delay <exp>) simply as (lambda() <exp>) then each
;;; time we're accessing stream there would be an additional addition
;;; thus every time n increases the number of addition rises
;;; exponentially.
