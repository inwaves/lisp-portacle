(defun count-change (amount)
  (cc amount 5))

(defun cc (amount koc)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= koc 0)) 0)
        ('t (+ (cc amount (- koc 1))
               (cc (- amount (first-denomination koc)) koc)))))

(defun first-denomination (koc)
  (cond ((= koc 1) 1)
        ((= koc 2) 2)
        ((= koc 3) 5)
        ((= koc 4) 10)
        ((= koc 5) 20)
        ((= koc 6) 50)))


(defun f-rec (n)
  (cond ((< n 3) n)
        ('t (+ (f-rec (- n 1)) (* 2 (f-rec (- n 2))) (* 3 (f-rec (- n 3)))))))

(defun f-iter ())

(defun pascal (n)
  (setf c-list ())
  (cond ((= n 1) (setf c-list (1)))
        ((= n 2) (setf c-list (1 1)))
        ('t (setf list (pascal (- n 1)))
            (setf list (generate-list ))
            (print list))))

