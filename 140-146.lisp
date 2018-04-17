;; exercises 1.40 - 1.46 from SICP


;; 1.40
(defparameter tolerance 0.00000001)
(defparameter dx 0.00001)

(defun fixed-point (f first-guess)
  (defun close-enough? (v1 v2)
        (< (abs (- v1 v2)) tolerance))
  (defun try (guess)
      (let ((next (funcall f guess)))
        (if (close-enough? guess next)
            next
            (try next))))
  (try first-guess))

(defun deriv (g)
  (lambda (x) (/ (- (funcall g (+ x dx)) (funcall g x)) dx)))

(defun newton-transform (g)
  (lambda (x)
    (- x (/ (funcall g x) (funcall (deriv g) x)))))

(defun newtons-method (g guess)
  (fixed-point (newton-transform g) guess))

(defun cubic (a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

;; 1.41
(defun procedure-doubler (f)
  (lambda (x) (funcall f (funcall f x))))

(defun inc (x)
  (+ x 1))

;; 1.42
(defun composition (f g)
  (lambda (x) (funcall f (funcall g x))))

;; 1.43
(defun repeated (f n)
  (defun iterate (n f res)
    (cond ((= n 1) res)
          (t (iterate (- n 1) f (composition f res)))))
  (iterate n f f)
  )

(defun sq (a)
  (* a a))

;; 1.44
;; combine smooth and repeat to do n-fold smooth
(defun average3 (a b c)
  (/ (+ a b c) 3))

(defun smooth (f)
  (lambda (x) (average3 (funcall f (- x dx)) (funcall f x) (funcall f (+ x dx)))))

(defun average2 (a b)
  (/ (+ a b) 2))

;; 1.45
(defun average-damp (f)
    (lambda (x) (average2 x (funcall f x))))

(defun cube-root (x)
  (fixed-point (average-damp (lambda (y) (/ x (sq y)))) 1.0))

(defun fast-expt (x n)
  (cond ((= n 0) 1)
        ((= (mod n 2) 0) (fast-expt (sq x) (/ n 2)))
        (t (* x (fast-expt x (- n 1))))))

(defun fourths (x)
  (fixed-point (repeated (average-damp (lambda (y) (/ x (* y y y)))) 5) 2.0))

;; seems to fail for n >= 5, I don't know why
(defun nth-roots (x n)
  (fixed-point (repeated (average-damp (lambda (y) (/ x (fast-expt y (- n 1))))) (- n 1)) 1.0))

;; 1.46

(defun iterative-improve (good-enough? improve)
  (lambda (guess) (cond ((good-enough? guess x) guess)
                    (t (lambda (improve guess x))))))

(defun good-enough? (guess x)
  (< (abs (- (sq guess) x)) 0.001))

(defun improve-sq (guess x)
  (average2 guess (/ x guess)))
