;; For n = 5
;; {(A 16) (B 8) (C 4) (D 2) (E 1)}
;; {(A 16) (B 8) (C 4) ({D E} 3)}
;; {(A 16) (B 8) ({C D E} 7)}
;; {(A 16) ({B C D E} 15)}
;; {({A B C D E} 31)}
;;
;;   {A B C D E} 31
;;       /\
;;      /  \
;;     /    \
;;   A 16   {B C D E} 15
;;          /\
;;         /  \
;;        /    \
;;      B 8    {C D E} 7
;;             /\
;;            /  \
;;           /    \
;;         C 4     {D E} 3
;;                 /\
;;                /  \
;;               /    \
;;             D 2   E 1
;;
;; The most frequent symbol requires 1 bit. The least frequent symbol 4 bits.
;;
;;
;; For n = 10
;; {(A 512) (B 256) (C 128) (D 64) (E 32) (F 16) (G 8) (H 4) (I 2) (J 1)}
;; {(A 512) (B 256) (C 128) (D 64) (E 32) (F 16) (G 8) (H 4) ({I J} 3)}
;; {(A 512) (B 256) (C 128) (D 64) (E 32) (F 16) (G 8) ({H I J} 7)}
;; {(A 512) (B 256) (C 128) (D 64) (E 32) (F 16) ({G H I J} 15)}
;; {(A 512) (B 256) (C 128) (D 64) (E 32) ({F G H I J} 31)}
;; {(A 512) (B 256) (C 128) (D 64) ({E F G H I J} 63)}
;; {(A 512) (B 256) (C 128) ({D E F G H I J} 127)}
;; {(A 512) (B 256) ({C D E F G H I J} 255)}
;; {(A 512) ({B C D E F G H I J} 511)}
;; {({A B C D E F G H I J} 1023)}
;;
;;   {A B C D E F G H I J} 1023
;;       /\
;;      /  \
;;     /    \
;;   A 512  {B C D E F G H I J} 511
;;          /\
;;         /  \
;;        /    \
;;      B 256  {C D E F G H I J} 255
;;             /\
;;            /  \
;;           /    \
;;         C 128  {D E F G H I J} 127
;;                 /\
;;                /  \
;;               /    \
;;             D 64   {E F G H I J} 63
;;                    /\
;;                   /  \
;;                  /    \
;;                E 32   {F G H I J} 31
;;                       /\
;;                      /  \
;;                     /    \
;;                   F 16   {G H I J} 15
;;                          /\
;;                         /  \
;;                        /    \
;;                      G 8    {H I J} 7
;;                             /\
;;                            /  \
;;                           /    \
;;                         H 4    {I J} 3
;;                                /\
;;                               /  \
;;                              /    \
;;                            I 2    J 1
;;
;; The most frequent symbol requires 1 bit. The least frequent symbol 9 bits.
