;;; With serializers there's no way processes running sequentially
;;; will interacti with each other in the middle of
;;; execution. Previous implementation (without serializers) might end
;;; up with incorrect values.
