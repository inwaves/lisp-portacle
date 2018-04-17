;; defining some prime utilities

;; 1 sqrt(n) growth algorithm
(defun prime? (n)
  (eq (find-divisor n 2) n))

(defun find-divisor (n divisor)
  (cond ((> (sq divisor) n) n)
        ((divides? divisor n) divisor)
        (t (find-divisor n (1+ divisor)))))

(defun divides? (a b) (= (mod b a) 0))

(defun sq (x) (* x x))

;; 2 log(n) growth probabilistic algorithm based on Fermat's test

(defun expmod (base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (mod (sq (expmod base (/ exp 2) m)) m))
        (t (mod (* base (expmod base (1- exp) m)) m))))

(defun fermat-test (n)
  (defun try-it (a)
    (= (expmod a n n) a))
  (cond ((= n 1) 1)
        (t  (try-it (+ 1 (random (- n 1))))))
  )

(defun fast-prime? (n times)
  (cond ((= times 0) t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (t ())))

(defun fprime? (n)
  (fast-prime? n 1000))

(defun relative-prime (a b)
  (eq (gcd a b) 1))
