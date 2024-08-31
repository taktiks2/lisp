(defvar a '(+ 1 2))

(car a)
(cdr a)

(car (cdr a))
(cdr (cdr a))

(car (cdr (cdr a)))
(cdr (cdr (cdr a)))

(eval a)

(macroexpand '(loop for i from 1 to 10 sum i))
(format t "Hello, World")
