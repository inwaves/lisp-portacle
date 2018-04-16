;;;; Welcome to Portacle, the Portable Common Lisp Environment.
;; For information on Portacle and how to use it, please read the website at
;;   https://github.com/Shinmera/portacle
;; or see the *portacle-help* buffer. You can switch to it by pressing this:
;;   Ctrl+h h
;; or by clicking on the *scratch* field below until it changes to read
;; *portacle-help*.
;;
;; You can use this buffer for notes and tinkering with small pieces of code.

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

(label maplist. (lambda (x f)
       (cond ((null x) '())
             ('t (cons (f x) (maplist. (cdr x)))))))
