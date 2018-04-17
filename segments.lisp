;; representations of lines and points

(defun make-line (start end)
  (cons start end))

(defun line-start (line)
  (car line))

(defun line-end (line)
  (cdr line))

(defun make-point (x y)
  "Defining a point using x and y coordinates"
  (cons x y))

(defun get-x (point)
  (car point))

(defun get-y (point)
  (cdr point))

(defun print-point (point)
  (format t "P(~d, ~d)~%" (get-x point) (get-y point)))

(defun middle (line)
  (cons (/ (+ (get-x (line-start line)) (get-x (line-end line))) 2)
        (/ (+ (get-y (line-start line)) (get-y (line-end line))) 2)))

;; representations of a rectangle:
;; 1) Start coordinates; length and width; builds a rectangle to the top-right of P(x0, y0)
;; 2) Coordinates of four points; validate that the distances are lengths and widths
;; in order to apply abstractions, both should return the 4 vertices of the rectangle.

(defun rect-lw (x0 y0 len wid)
  (let ((p1 (make-point x0 y0))
        (p2 (make-point (+ x0 len) y0))
        (p3 (make-point x0 (+ y0 wid)))
        (p4 (make-point (+ x0 len) (+ y0 wid))))
    (cons p1 (cons p2 (cons p3 (cons p4 nil))))))

(defun perimeter (rect)
  (+ (* 2 (- (get-x (cadr rect)) (get-x (car rect)))) (* 2 (- (get-y (caddr rect)) (get-y (cadr rect))))))

(defun area (rect)
  )

(defun rect-points (p1 p2 p3 p4)
  
  )

