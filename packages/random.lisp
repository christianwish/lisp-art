(defun random-number-to (end)
  (progn
    (setf *random-state* (make-random-state t))
    (random (max end 1))))

;; TODO
(defun random-number-from-to (n m &key as-float never-zero)
  (let* ((start (min n m))
         (end (max n m))
         (both-negtive (and (< start 0) (< end 0)))
         (start-negativ (and (< start 0) (>= end 0)))
         (from-zero
            (cond (both-negtive (- (abs start) (abs end)))
                  (start-negativ (+ (abs start) end))
                  (t (- end start))))
         (result1 (random-number-to from-zero))
         (result2
            (cond (both-negtive (- end result1))
                  (start-negativ (+ result1 start))
                  (t (+ result1 start))))
         (is-zero (and never-zero (= result2 0)))
         (result3
            (if is-zero
                (random-number-from-to m n :never-zero t)
                result2)))
       (if as-float (float result3) result3)))

(defun random-1-or-2 (&optional as-float)
  (let ((result (if (oddp (random-number-to 10)) 1 2)))
       (if as-float (float result) result)))

(defmacro random-item-via-weight (&rest all)
  "gives an random item. Where weight (int) influences the possibility."
  `(let* ((random-list (list)) ;; empty list
          (random-list-length
            (reduce
              #'(lambda (acc x)
                  (dotimes (i (round (second x))) (push (first x) random-list));; push
                  (+ acc (round (second x))))
                ',all
                :initial-value 0))
        )
        (nth (random-number-to (length random-list)) random-list))
  )
