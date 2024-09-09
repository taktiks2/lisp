(defconstant +board-height+ 30)
(defconstant +board-width+ 90)

(defun gen-rand-num ()
  (random 2))

(defun render-board ()
  (dotimes (i +board-height+)
    (dotimes (j +board-width+)
      (princ (gen-rand-num)))
    (terpri)))

(defun main ()
  (render-board))

(main)
