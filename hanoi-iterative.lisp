;; this is an iterative implementation of the towers of hanoi puzzle
;; hardcoded to 4 disks; 31 moves needed

(defun move-disk (source dest)
  (cond ((eq (car source) 1) (setf (car source) 0))
        ((eq (car source) 1)  (setf (cadr source) 0))
        ((eq (car source) 1)  (setf (caddr source) 0))
        ((eq (car source) 1)  (setf (cadddr source) 0)))
  (cond ((eq (cadddr dest) 0) (setf (cadddr dest) 1))
        ((eq (caddr dest) 0) (setf (caddr dest) 1))
        ((eq (cadr dest) 0) (setf (cadr dest) 1))
        ((eq (car dest) 0) (setf (car dest) 1))
        ))

(defun hanoi ()
  (defvar source '(1 1 1 1))
  (defvar aux '(0 0 0 0))
  (defvar dest '(0 0 0 0))

  (loop for moves from 1 to 31
        do (format t "Move ~d" moves)
            (cond ( (eq (mod moves 3) 1) 
                                         (move-disk source dest))
                 ( (eq (mod moves 3) 2) 
                   (move-disk source aux))
                 ( (eq (mod moves 3) 0)
                   (move-disk aux dest)))
        (format t "src = ~d ~% aux = ~d ~% dest = ~d ~%" source aux dest)
        ))
