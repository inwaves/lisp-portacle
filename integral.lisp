;; implementing integrals through higher-order procedures

"
(defun sum (term a next b)
  (cond ((> a b) 0)
        (t (+ (term a) (sum (funcall term) (next a) (funcall b) b)))))

(defun sq (a)
  (* a a))

(defun integral (f a b dx)
  
  (* (sum f (+ a (/ dx 2.0)) add-dx b))
  )

(defun add-dx (x dx) (+ x dx))
"

(defun plot (fn min max step)
  (loop for i from min to max by step do
        (loop repeat (funcall fn i) do (format t "*"))
        (format t "~%")))

(defun simpson (f a b n)
  (let ((h (/ (- b a) n)))
    (* (/ h 3) (iterate a 0 n f h 0))))

(defun term (f a h n k)
  (cond ((or (= k 0) (= k n)) (funcall f (+ a (* k h))))
        ((even? k) (* 2 (funcall f (+ a (* k h)))))
        (t (* 4 (funcall f (+ a (* k h)))))))

(defun even? (x)
  (eq (mod x 2) 0))

(defun iterate (a k n f h s)
  (cond ((<= k n) (iterate a (1+ k) n f h (+ s (term f a h n k))))
        (t s)
        ))

