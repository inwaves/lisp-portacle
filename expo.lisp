;; different methods of exponentiation

(defun expt1 (b n)
  (cond ((> n 0) (* b (expt1 b (- n 1))))
        (t 1)))

(defun sq (b)
  (* b b))

(defun fast-expt (b n)
  (cond ((= n 0) 1)
        ((even? n) (sq (fast-expt b (/ n 2))))
        (t (* b (fast-expt b (- n 1))))))

(defun even? (n)
  (eq (mod n 2) 0))

(Defun fast-expt-iter (b n)
  (defparameter a 1)
  (defun iterate (a b n)
    (cond ((> n 1) (cond ((even? n) (setf a (* a (sq b)))
                          (setf n (/ n 2))
                          (iterate a b n))
                         (t (setf a (* a b))
                            (iterate a b (- n 1)))))
          (t a))
    )
  (iterate a b n))

(defun double-number (a)
  (+ a a))

(defun halve-number (a)
  ())
