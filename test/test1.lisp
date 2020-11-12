(defpackage :test1 (:use :common-lisp) (:export #:specs))

(in-package :test1)

(defun specs ()
    (progn
        (print "PASS...")
    ))
