;; SICP: Chapter 2, 2.7-2.16

(defun width (x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

(defun add-interval (x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defun mul-interval (x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


(defun div-interval (x y)
  (cond ((= 0 (width y)) () (format t "Cannot divide by width 0"))
        (t  (mul-interval x 
            (make-interval (/ 1.0 (upper-bound y))
                           (/ 1.0 (lower-bound y)))))))

(defun make-interval (a b) (cons a b))

(defun upper-bound (interval) (cdr interval))

(defun lower-bound (interval) (car interval))

(defun sub-interval (x y)
  (make-interval (- (lower-bound x) (upper-bound y)) (- (upper-bound x) (lower-bound y))))

(defun mul-interval-endpoints (x y)
  )
