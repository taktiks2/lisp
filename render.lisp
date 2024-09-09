(defun render-board ()
  (dotimes (i 10)
    (dotimes (j 10)
      (princ 0)) ; NOTE: 改行なしの出力
    (terpri))) ; NOTE: 改行

(render-board)
