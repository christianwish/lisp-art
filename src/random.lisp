(defun random-number-to (end)
  (progn
    (setf *random-state* (make-random-state t))
    (random (max end 1))))

(defun random-number-between (start end)
  (+ (random-number-to (- end start)) start))
