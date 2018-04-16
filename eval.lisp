;; this is a full Lisp evaluator built with 7 primitive types: atom, quote
;; cons, car, cdr and cond. The helper functions are
(defun eval. (e a)
  (cond
    ((atom e) (assoc. e a))
    ((atom (car e))
     (cond
       ((eq (car e) 'quote) (cadr e))
       ((eq (car e) 'atom)  (atom (eval. (cadr e) a)))
       ((eq (car e) 'eq)    (eq (eval. (cadr e) a)
                                (eval. (caddr e) a)))
       ((eq (car e) 'car)   (car (eval. (cadr e) a)))
       ((eq (car e) 'cdr)   (cdr (eval. (cadr e) a)))
       ((eq (car e) 'cond)  (evcon. (cdr e) a))
       ('t (eval. (cons (assoc. (car e) a)
                         (cdr e))
                   a))))
    ((eq (caar e) 'label)
     (eval. (cons (caddar e) (cdr e))
            (cons (list (cadar e) (car e)) a)))
    ((eq (caar e) 'lambda)
     (eval. (caddar e)
            (append. (pair. (cadar e) (evlis. (cdr e) a))
                     a)))))
      


(defun append. (x y)
  (cond ((car x) (append. (cdr x) y))
        ('t y)))

(defun and. (x y)
  (cond (x (cond (y 't) ('t ())))
        ('t ())))

(defun pair. (x y)
  (cond ((and (car x) (car y))  (cons (list (car x) (car y)) (pair. (cdr x) (cdr y))))
        ('t ()))
  )

(defun assoc. (x y)
  (cond ((eq (caar y) x) (cadar y))
        ('t (assoc. x (cdr y)))))

(defun null. (x)
  (eq x '()))

(defun evcon. (c a)
  (cond ((eval. (caar c) a)
         (eval. (cadar c) a))
        ('t (evcon. (cdr c) a))))

(defun evlis. (m a)
  (cond ((null. m) '())
        ('t (cons (eval. (car m) a)
                  (evlis. (cdr m) a)))))
