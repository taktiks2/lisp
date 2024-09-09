(defparameter *small* 1)
(defparameter *big* 100)

(defun guess-number ()
  (ash (+ *small* *big*) -1))
