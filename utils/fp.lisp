(defpackage :fp
    (:use :common-lisp)
    (:export
        #:composition-2
        #:composition-3
        ))

(in-package :fp)

(defmacro composition-2 (fnName f1 f2)
    `(defun ,fnName (x) (,f1(,f2 x))))

(defmacro composition-3 (fnName f1 f2 f3)
    `(defun ,fnName (x) (,f1(,f2 (,f3 x)))))

(defmacro composition-5 (fnName f1 f2 f3 f4 f5)
    `(defun ,fnName (x) (,f1(,f2 (,f3 (,f4 (,f5 x)))))))
