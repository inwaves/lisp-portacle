;; finding fixed points of functions

(defparameter tolerance 0.0001)

(defun fixed-point (f first-guess)
  (format t "tolerance = ~f~%" tolerance)
  (defun close-enough? (v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (defun try (guess)
    (let ((next (funcall f guess)))
      (format t "guess = ~f, next = ~f~%" guess next)
      (cond ((close-enough? guess next) next)
            (t (try next)))))
  (try first-guess))

(defun f (x)
  (cond ((= x 0) 1)
        (t (+ 1 (/ 1 x)))))

(defun logs (x)
  (cond ((= x 0) 1)
        (t (/ (+ x (/ (log 1000) (log x))) 2))))

(defun n (i) i)
(defun d (i) i)

(defun nphi (i) 1.00)
(defun dphi (i) 1.00)

(defun deuler (i dp)
  (cond ((= (mod i 3) 0) dp)
        (t 1)))

(defun deuler-wrapper (i))

(defun fraction (n d k)
  (defun calc-fraction (n d i)
    (/ (funcall n i) (+ (funcall d i) (cond ((= i (- k 1)) (/ (funcall n k) (funcall d k)))
                                            ('t (calc-fraction n d (1+ i)))))))
  ;;  (calc-fraction n d 1)
  (defun calc-fraction-iterative ()
    (defun iterate (i k)
      (cond ((= k i) (defparameter result (/ (funcall n k) (funcall d k))))
            (t (defparameter result (/ (funcall n i) (+ (funcall d i) result)))))
      (cond ((= i 1) result)
            (t (iterate (1- i) k)))
      )
    (iterate k k))
  (defparameter dp 4)
  (calc-fraction-iterative)
  )

