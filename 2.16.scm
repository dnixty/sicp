;; Equivalent algebraic expressions may lead to different answers
;; because the tolerances in intervals for the sum and difference are
;; not the same as for multiplication or division. Multiplication and
;; division introduced higher tolerance while sum and division doesn't
;; change the tolerance at all in comparison to the individual pieces
;; of the computation.
;; It's possible to devise an interval-arithmetic package that does
;; not have this shortcomings. This package would have to have an
;; ability to simplify operations to have the least amount of
;; multiplication and division in favor of sumation and subtraction.
