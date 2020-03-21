;;; Let's assume two processes are trying to acquire the mutex. The
;;; first process calls test-and-set!, seeing that the (car cell) is
;;; false. The second process is doing the same. The first process now
;;; sets the (car cell) to true. And now the second process is doing
;;; the same. This is incorrect behaviour.
