(defpackage :random
  (:use :common-lisp)
  (:export
   #:number-to
   ))

(in-package :random)

(defun number-to (end)
  (progn
    (setf *random-state* (make-random-state t))
    (random end)))
