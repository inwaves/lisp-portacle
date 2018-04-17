;; fast exponentiation through successive squaring algorithms

(defun square (n)
  (* n n))

(defun fast-expt (b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (t (* b (fast-expt b (- n 1))))))

(defun even? (n)
  (= (mod n 2) 0))

(defun fast-expt-iter (a b n)
  (loop while (> n 1)
        do()))
