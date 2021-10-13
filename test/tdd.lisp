(defpackage :tdd (:use :common-lisp) (:export #:deftest))

(defmacro make-format-color (&rest colors)
  (let ((color-fn #'(lambda (color)
    (let ((fn-name (read-from-string (concatenate 'string "format-" (first color))))
      (color-code (second color)))
      `(defun ,fn-name (txt)
        (format nil (concatenate 'string "~c[" ,color-code "m" txt "~c[0m") #\ESC #\ESC))))))
    `(progn ,@(mapcar color-fn colors))))

(make-format-color ("cyan" "96") ("green" "32") ("red" "31") ("yellow" "33"))

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
      (terpri t))))

(defmacro deftest (test-name &key describe tests func)
  (let (
    (test-title (string test-name))
    (function-title (when func (string-downcase (string func))))
    (to-check #'(lambda (all-checks) `(check ,@all-checks)))
    )
    `(defun ,test-name ()
      (terpri t)
      ;;(format t (format-cyan ,test-title))
      (when ,describe (terpri t))
      (when ,function-title (format t "[fun: ~a]" ,function-title))
      (when ,function-title (terpri t))
      (if ,describe (format t (format-yellow ,describe)) (terpri t))
      (when ,describe (terpri t))
      ,@(mapcar to-check tests)))
  )
