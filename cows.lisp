;; Start with a list containing 0.0, representing the first cow
;; the whole amount is the number of cows spawned from that cow
;; the decimal amount is the maturity and will range 0->3
;; at every tick, increment maturity (so add 0.1 to cow) and year count
;; when maturity reaches 3, increment the whole amount of that cow and spawn new 0.0 element
;; continue until year count reaches parameter

(defun cows (years)
  (list (defvar *mycows* (list 0.0))
        (loop for elapsed from 1 to years
              do (loop for counter from 0 to (- (list-length *mycows*) 1)
                       do (if (>= (nth counter *mycows*) 0.25)
                              (list (incf (nth counter *mycows*))
                                    (append *mycows* (list 0.0)))
                              (setf (nth counter *mycows*) (+ (nth counter *mycows*) 0.1))))
                 (format t "list = ~S" *mycows*))))
