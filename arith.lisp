;; basic arithmetic using linear- and logarithmic-order algorithms

(defun fast-mult (a b)
  (cond ((= b 0) 0)
        (t (cond ((even? b) (+ (doubler a) (fast-mult a (- b 2))))
                 (t (+ a (fast-mult a (- b 1))))
                 ))))

(defun fast-mult-iterative (a b)
  (defun iterate (a b n)
    (cond ((= b 0) n)
          (t (cond ((even? b) (setf n (+ n (doubler a))) (iterate a (- b 2) n))
                   (t (setf n (+ n a)) (iterate a (1- b) n))))))
  (iterate a b 0))

(defun mult (a b)
  (cond ((= b 0) 0)
        ('t (+ a (mult a (- b 1))))))

(defun doubler (a)
  (+ a a))

(defun halve (a)
  (/ a 2))

(defun even? (a)
  (eq (mod a 2) 0))

;; logarithmic-growth fibonacci using an observation on the transformation from a pair to the other
;; Tpq transforms the pair (a, b): a = bq + aq + ap, b = bp + aq, for Fibonacci p = 0, q = 1
;; if we apply this twice we get a transformation where p = q = 1, almost like squaring a number for
;; fast exponentiation

(defun sq (a)
  (* a a))

(defun fib (n)
  (fib-iter 1 0 0 1 n))

(defun fib-iter (a b p q count)
  (cond ((= count 0) b)
        ((even? count) (fib-iter a b (+ (* p p) (* q q)) (+ (* q q) (* 2 p q)) (/ count 2)))
         (t (fib-iter (+ (* b q) (* a q) (* a p)) (+ (* b p) (* a q)) p q (- count 1)))))

(defun smallest-divisor (n)
  (find-divisor n 2))

(defun find-divisor (n test-divisor)
  (cond ((> (sq test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (t (find-divisor n (+ test-divisor 1)))))

(defun divides? (a b)
  (= (mod b a) 0))

(defun expmod (base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (mod (sq (expmod base (/ exp 2) m)) m)
         (t (mod (* base (expmod base (- exp 1) m)) m)))))
