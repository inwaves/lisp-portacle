(defun fact (n)
  (if (= n 1)
      1
      (* n (fact (- n 1)))))

(defun out (n)
  (if (> n 0)
      (list
       (format t "~d! = ~d~%" n (fact n))
       (decf n)
       (out n))
      1))

  
