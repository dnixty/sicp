;;; RECURSIVE VERSION
;;; =================

;;; GLOBAL ENV
;;; factorial
;;; variables: n
;;; (if (= n 1) 1 (* n (factorial (- n 1))))

;;; (factorial 6)

;;; E1->GLOBAL ENV
;;; n:6
;;; (* 6 (factorial 5))

;;; E2->GLOBAL ENV
;;; n:5
;;; (* 5 (factorial 4))

;;; E3->GLOBAL ENV
;;; n:4
;;; (* 4 (factorial 3))

;;; E4->GLOBAL ENV
;;; n:3
;;; (* 3 (factorial 2))

;;; E5->GLOBAL ENV
;;; n:2
;;; (* 2 (factorial 1))

;;; E6->GLOBAL ENV
;;; n:1
;;; 1


;;; ITERATIVE VERSION
;;; =================

;;; GLOBAL ENV
;;; factorial
;;; variables: n
;;; (fact-iter 1 1 n)
;;;
;;; fact-iter
;;; variables: product, counter, max-count
;;; (if (> counter max-count) product (fact-iter (* counter product) (+ counter 1) max-count))

;;; (factorial 6)

;;; E1->GLOBAL ENV
;;; n:6
;;; (fact-iter 1 1 n)

;;; E2->GLOBAL ENV
;;; product:1
;;; counter:1
;;; max-count:6
;;; (fact-iter 1 2 6)

;;; E3->GLOBAL ENV
;;; product:1
;;; counter:2
;;; max-count:6
;;; (fact-iter 2 3 6)

;;; E4->GLOBAL ENV
;;; product:2
;;; counter:3
;;; max-count:6
;;; (fact-iter 6 4 6)

;;; E5->GLOBAL ENV
;;; product:6
;;; counter:4
;;; max-count:6
;;; (fact-iter 24 5 6)

;;; E6->GLOBAL ENV
;;; product:24
;;; counter:5
;;; max-count:6
;;; (fact-iter 120 6 6)

;;; E7->GLOBAL ENV
;;; product:120
;;; counter:6
;;; max-count:6
;;; (fact-iter 720 7 6)

;;; E8->GLOBAL ENV
;;; product:720
;;; counter:7
;;; max-count:6
;;; 720
