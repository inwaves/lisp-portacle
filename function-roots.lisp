;; half-interval method

(defun search-interval (f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (cond ((close-enough? neg-point pos-point) midpoint)
          (t (let ((test-value (funcall f midpoint)))
                   (cond ((< 0 test-value) (search-interval f neg-point midpoint))
                         ((> 0 test-value) (search-interval f midpoint pos-point))
                         (t midpoint)))))))

(defun average (a b)
  (/ (+ a b) 2))

(defun close-enough? (a b)
  (< (- b a) 0.0001))

(defun half-interval-method (f a b)
  (let ((a-value (funcall f a))
        (b-value (funcall f b)))
    (cond ((and (> 0 a-value) (< 0 b-value)) (search-interval f a b))
          ((and (< 0 a-value) (> 0 b-value)) (search-interval f b a))
          (t (format t "Values are not of opposite sign.")))))
