(defpackage :tdd
    (:use :common-lisp)
    (:export
        #:test
        #:it
        ))

(in-package :tdd)

(defmacro make-format-color (&rest colors)
  "generates helper functions for printing colorful output"
  (let ((color-fn #'(lambda (color)
    (let
      ((fn-name (read-from-string (concatenate 'string "format-" (first color))))
        (color-code (second color)))
      `(defun ,fn-name (txt)
        (format nil (concatenate 'string "~c[" ,color-code "m" txt "~c[0m") #\ESC #\ESC))))))
    `(progn ,@(mapcar color-fn colors))))

(make-format-color
  ("cyan" "96")
  ("green" "32")
  ("purple" "35")
  ("yellow" "33")
  ("red" "31"))

(defun run (result)
    (format t "... ")
    (if (second result)
        (format t (format-green (first result)))
        (format t (format-red (first result))))
    (terpri t))

(defun test (msg &rest results)
    (progn
        (terpri)

        (format t (format-cyan msg))
        (terpri t)
        (mapcar 'run results)
        (terpri t)))

(defun it (msg x)
    (list msg x))
