;; Lisp implementation of Pascal's triangle using the base definition
;; i.e. each number is the sum of the two directly above it



(defun generate-iterative (prev-list)
  (defparameter li '(0))
  (loop for ctr from 0 to (list-length prev-list) do
        (cond ((= ctr (list-length prev-list)) (setf (nth ctr li) 1))
              ('t (setf li (append li '(0))))))
  
  (loop for ctr from 0 to (- (list-length prev-list) 1)
        do (cond ((= ctr 0) (setf (nth ctr li) 1))
                 ('t (setf (nth ctr li) (+ (nth (- ctr 1) prev-list) (nth ctr prev-list))))
                 ))
  (defun printlist (li)
    (format t "~d" (car li))
    (printlist (cdr li)))
  (printlist li)
  )

(defun pascal (n)
  (cond ((= n 1) '(1) (print '(1)))
        ('t (generate-iterative (pascal (- n 1))))))

 

