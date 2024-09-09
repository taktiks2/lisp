;; NOTE: applyの使い方
; NOTE: #は与えられたシンボルの関数を返す
(print (apply #'+ '(1 2 3)))

;; NOTE: funcallの使い方
(print (funcall #'+ 4 5 6))

;; NOTE: ラムダ関数
(print ((lambda (x) (* x x)) 2))

