;; rational arithmetic

(defun make-rat (x y)
  (cond ((< x 0) (cond ((< y 0) (setf x (* x -1)) (setf y (* y -1)))))
        ((< y 0) (setf y (* y -1)) (setf x (* x -1))))
  (let ((g (gcd x y)))
    (cons (/ x g) (/ y g))))

(defun num (rat)
  (car rat))

(defun denom (rat)
  (cdr rat))

(defun print-rat (rat)
  (format t "~d/~d~%" (num rat) (denom rat)))

(defun add-rat (rat1 rat2)
  (/ (+ (* (num rat1) (denom rat2)) (* (num rat2) (denom rat1))) (* (denom rat1) (denom rat2))))

(defun mult-rat (rat1 rat2)
  (/ (* (num rat1) (num rat2)) (* (denom rat1) (denom rat2))))

(defun my-gcd (a b)
  (cond ((= b 0) a)
        (t (gcd b (mod a b)))))
