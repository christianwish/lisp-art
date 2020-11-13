(defpackage :io
    (:use :common-lisp)
    (:export
        #:labeled-read-ln
        ))

(in-package :io)

(defun labeled-read-ln (label)
    (format *query-io* "~a: " label)
    (force-output *query-io*)
    (read-line *query-io*))
