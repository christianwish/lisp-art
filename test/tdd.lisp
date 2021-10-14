(defpackage :tdd (:use :common-lisp) (:export #:deftest))

(defmacro defcolor (function-name color-code)
  `(defun ,function-name (str)
    (format nil (concatenate 'string "~c[" ,(write-to-string color-code) "m" str "~c[0m") #\ESC #\ESC)))

(defcolor format-cyan 96)
(defcolor format-yellow 33)
(defcolor format-red 31)
(defcolor format-green 32)

(defun check (&optional &key test actual expected (is-true nil true-check))
  (if true-check
    (let* (
        (format-fn (if is-true #'format-green #'format-red)))
      (format t "*  ")
      (format t (funcall format-fn test))
      (terpri t))
    (let* (
        (result (equal actual expected))
        (format-fn (if result #'format-green #'format-red)))
      (format t "*  ")
      (format t (funcall format-fn test))
      (terpri t)
      (when (not result) (print actual)))))

(defmacro deftest (test-name &key describe tests func)
  (let (
    (test-title (string test-name))
    (function-title (when func (string-downcase (string func))))
    (to-check #'(lambda (all-checks) `(check ,@all-checks)))
    )
    `(defun ,test-name ()
      (terpri t)
      (format t (format-cyan ,test-title))
      (when ,describe (terpri t))
      (when ,function-title (format t "[fun: ~a]" ,function-title))
      (when ,function-title (terpri t))
      (if ,describe (format t (format-yellow ,describe)) (terpri t))
      (when ,describe (terpri t))
      ,@(mapcar to-check tests)))
  )
