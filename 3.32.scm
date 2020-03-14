;;; The order of signal changes is important even in the same time
;;; segment. If procedures for each segment are not called in the FIFO
;;; order, simulation will not be done in the right order and it
;;; causes wrong results.
