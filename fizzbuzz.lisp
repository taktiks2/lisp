(defun fizzbuzz (n)
          (cond
            ((and (= (mod n 5) 0) (= (mod n 3) 0))
             (print 'fizzbuzz))
            ((= (mod n 5) 0) 'buzz)
            ((= (mod n 3) 0) 'fizz)
            (t n)))

;; NOTE: 再帰関数での実装
(defun recursion-fizzbuzz (start end)
  (unless (> start end)
    (print (fizzbuzz start))
    (recursion-fizzbuzz (1+ start) end)))

;; NOTE: ループ1での実装
(defun dotimes-fizzbuzz (n)
  (dotimes (i n)
    (print (fizzbuzz (1+ i)))))

;; NOTE: ループ2での実装
(defun loop-fizzbuzz (n)
  (let ((i 1))
    (loop
      (if (> i n) (return))
      (print (fizzbuzz i))
      (incf i))))

(recursion-fizzbuzz 1 20)
(dotimes-fizzbuzz 20)
(loop-fizzbuzz 20)
