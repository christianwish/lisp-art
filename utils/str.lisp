(defpackage :str
    (:use :common-lisp)
    (:export
        #:concat-all
        ))

(in-package :str)

(defun concat-all (&rest xs)
    (if (= (list-length xs) 0)
        ""
        (concatenate 'string (first xs)
            (apply #'concat-all (rest xs) ))))
