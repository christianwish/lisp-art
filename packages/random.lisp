(defun random-number-to (end)
  (progn
    (setf *random-state* (make-random-state t))
    (random (max end 1))))

(defun random-number-between (start end &optional as-float)
  (let ((result (+ (random-number-to (- end start)) start)))
       (if as-float (float result) result)))

(defun random-1-or-2 (&optional as-float)
  (let ((result (if (oddp (random-number-to 10)) 1 2)))
       (if as-float (float result) result)))

(defmacro random-item-via-weight (&rest all)
  "gives an random item. Where the weight influences the random picking."
  `(let* ((random-list (list)) ;; empty list
          (random-list-length
            (reduce
              #'(lambda (acc x)
                  (dotimes (i (second x)) (push (first x) random-list));; push
                  (+ acc (second x)))
                ',all
                :initial-value 0))
        )
        (nth (random-number-to (length random-list)) random-list))
  )
