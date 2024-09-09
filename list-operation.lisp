;; NOTE: 再帰関数を定義
(defun fact (x)
  (if (zerop x)
      1
      (* x (fact (1- x)))))

(trace fact)
(print (fact 5))
(untrace fact)

;; NOTE: リストの要素数を出す関数
(defun my-length (x)
  (if (atom x)
      0
      (1+ (my-length (cdr x)))))

(trace my-length)
(print (my-length '(1 2 3 4 5)))
(untrace my-length)

;; NOTE: リストの先頭から指定した個数の要素を消していく関数
(defun drop (xs n)
  (if (or (null xs) (zerop n))
      xs
      (drop (cdr xs) (1- n))))

(trace drop)
(print (drop '(1 2 3 4 5) 3))
(untrace drop)

(defun my-append (xs ys)
  (if (null xs)
      ys
      (cons (car xs) (my-append (cdr xs) ys))))

(trace my-append)
(print (my-append '(1 2 3 4 5) '(3 3 2)))
(untrace my-append)
