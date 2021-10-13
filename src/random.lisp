(defun random-number-to (end)
  (progn
    (setf *random-state* (make-random-state t))
    (random end)))

(defun random-number-between (start end)
  (+ (number-to (- end start)) start))
