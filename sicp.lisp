;; following along SICP 1986

(defun sum-int (a b)
  (cond ((> a b) 0)
         ('t (+ a (sum-int (1+ a) b)))))

(defun sum-sq (a b)
  (cond ((> a b) 0)
        ('t (+ (sq a) (sum-sq (1+ a) b)))))

(defun sq (a)
  * a a)

(defun sum. (term a next b)
  (cond ((> a b) 0)
        (+ (term a) (sum term (next a) next b))))

(defun 1+. (a) (1+ a))

(defun sum-int. (a b)
  (defun identity (a) a)
  (sum. identity a '1+. b))

(defun sum-sq. (a b)
  (sum. 'sq a '1+ b))

(defun pi-sum (a b)
  (sum. (lambda (i) (/ 1 (* i (+ i 2))))
       a
       (lambda (i) (+ i 4))
       b))


(defun iterative-sum (term a next)
  "This should be an iterative implementation of 
   the sum. function above."
  (defun iter (j ans)
    (cond  ((> j b) ans)
           ('t (iter (next j) (+ (term j) ans)))))
  (iter a s0))
