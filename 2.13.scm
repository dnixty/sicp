;; Let w(X) - width of interval X
;;     c(X) - center of interval X
;;     p(X) - percentage of tolerance of interval X

;; w(X) = (xu - xl) / 2
;; c(X) = (xl + xu) / 2
;; p(X) = (xu - xl) / (xl + xu)
;; p(X) * (xl + xu) = xu - xl
;; (p(X) * xl) + (p(X) * xu) = xu - xl
;; (p(X) * xl) + xl = xu - (p(X) * xu)
;; xl * (p(X) + 1) = xu * (1 - p(X))
;; xl / xu = (1 - p(X)) / (p(X) + 1)
;; xl = xu * (1 - p(X)) / (p(X) + 1)

;; w(Y) = (yu - yl) / 2
;; c(Y) = (yl + yu) / 2
;; p(Y) = (yu - yl) / (yl + yu)
;; yl / yu = (1 - p(Y)) / (p(Y) + 1)
;; yl = yu * (1 - p(Y)) / (p(Y) + 1)

;; X * Y = (xl*yl, xu*yu)
;; w(X * Y) = (xu*yu - xl*yl) / 2
;; c(X * Y) = (xl*yl + xu*yu) / 2
;; p(X * Y) = (xu*yu - xl*yl) / (xl*yl + xu*yu)
;; p(X * Y) = (xu*yu - xu*yu*((1 - p(Y)) * (1 - p(X)) / ((p(Y) + 1) * (p(X) + 1)))) / (xu*yu + xu*yu*((1 - p(Y)) * (1 - p(X)) / ((p(Y) + 1) * (p(X) + 1))))
;; p(X * Y) = (1 - ((1 - p(Y) - p(X) + p(X)*p(Y)) / (p(X)*p(Y) + p(X) + p(Y) + 1))) / (1 + ((1 - p(Y) - p(X) + p(X)*p(Y)) / (p(X)*p(Y) + p(X) + p(Y) + 1)))
;; p(X * Y) = ((p(X)*p(Y) + p(X) + p(Y) + 1 - 1 + p(Y) + p(X) - p(X)*p(Y)) / (p(X)*p(Y) + p(X) + p(Y) + 1)) / ((p(X)*p(Y) + p(X) + p(Y) + 1 + 1 - p(Y) - p(X) + p(X)*p(Y)) / (p(X)*p(Y) + p(X) + p(Y) + 1))
;; p(X * Y) = (p(X) + p(Y)) / (p(X)*p(Y) + 1)
