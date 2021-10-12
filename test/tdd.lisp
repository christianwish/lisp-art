(defpackage :tdd (:use :common-lisp) (:export #:deftest))

(defmacro make-format-color (&rest colors)
  (let ((color-fn #'(lambda (color)
    (let ((fn-name (read-from-string (concatenate 'string "format-" (first color))))
      (color-code (second color)))
      `(defun ,fn-name (txt)
        (format nil (concatenate 'string "~c[" ,color-code "m" txt "~c[0m") #\ESC #\ESC))))))
    `(progn ,@(mapcar color-fn colors))))

(make-format-color ("cyan" "96") ("green" "32") ("red" "31"))

(defun check (&key test actual expected)
  (let* (
      (result (= actual expected))
      (format-fn (if result #'format-green #'format-red)))
    (format t "   ")
    (format t (funcall format-fn test))
    ;; (when (not result) (print actual))
    (terpri t)))

(defmacro deftest (function-name &key describe tests)
  (let (
    (function-title (string function-name))
    (to-check #'(lambda (all-checks) `(check ,@all-checks)))
    )
    `(defun ,function-name ()
      (terpri t)
      (format t (format-cyan ,function-title))
      (if ,describe (format t ": ~a" (format-cyan ,describe)) (terpri t))
      (when ,describe (terpri t))
      ,@(mapcar to-check tests)))
  )
