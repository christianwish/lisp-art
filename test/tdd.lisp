(defpackage :tdd
    (:use :common-lisp)
    (:export
        #:describe
        ))

(in-package :tdd)

(defmacro describe (fnName f1 f2)
    `(defun ,fnName (x) (,f1(,f2 x))))
