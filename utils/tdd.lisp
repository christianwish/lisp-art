(defpackage :tdd
    (:use :common-lisp)
    (:export
        #:test
        #:it
        ))

(in-package :tdd)

(require "str" "./utils/str.lisp")

(defun format-with-nl (&rest xs)
    (progn (format t (apply 'str:concat-all xs))
        (terpri)))

(defun run (result)
    (if (second result)
        (format-with-nl "   🌱 " (first result))
        (format-with-nl "   🔴 " (first result))))

(defun test (msg &rest results)
    (progn
        (terpri)
        (format-with-nl "▶️ [" msg "]")
        (mapcar 'run results)
        (terpri)))

(defun it (msg x)
    (list msg x))
