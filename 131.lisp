;; exercises 1.30-1.33 from SICP

;; 1.30
(defun sum (term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(defun sum-iter (term a next b)
  (defun iter (a result)
    (if (> a b)
        result
        (iter (funcall next a) (+ result (funcall term a)))))
  (iter a 0))

(defun id (x) x)

(defun inc (x) (1+ x))

(defun sq (x) (* x x))

;; 1.31

(defun prod (term a next b)
  (cond ((> a b) 1)
        (t (* (funcall term a)
              (prod term (funcall next a) next b)))))

(defun prod-rec (term a next b)
  (cond ((> a b) 1)
        (t (* (prod term (funcall next a) next b)
            (funcall term a)))))

(defun pi-term (k)
  (/ (* (* 2 k) (* 2 (+ k 1))) (sq (+ (* 2 k) 1))))

;; 1.32

;; the following are recursive and iterative respectively due to how the function call
;; to the next level occurs: for recursive we defer the operation until the very end,
;; so (funcall term a)  gets calculated for the first time when a = b.
(defun accumulate-rec (combiner null-value term a next b)
  (cond ((> a b) null-value)
        (t (funcall combiner (accumulate combiner null-value term (funcall next a) next b)
                    (funcall term a)))))

;; this one, however, computes (funcall term a) on the go, which is why it occurs before
;; the next function call. This is, in essence, the difference between an iterative and
;; a recursive process: either operations are deferred until later, or they are done partially
;; as the process goes on.
(defun accumulate-iter (combiner null-value term a next b)
  (cond ((> a b) null-value)
        (t (funcall combiner (accumulate-iter combiner null-value term (funcall next a) next b)
                    (funcall term a)))))

(defun summer (x y) (+ x y))

(defun multiplier (x y) (* x y))

(defun even? (x) (eq (mod x 2) 0))

(defun accumulate-iter-filter (filter combiner null-value term a next b)
  (cond ((> a b) null-value)
        (t  (cond ((funcall filter a b) 
                   (funcall combiner (accumulate-iter-filter filter combiner null-value term (funcall next a) next b) (funcall term a)))
                  ('t (accumulate-iter-filter filter combiner null-value term (funcall next a) next b))
                  )
        )))

