(defpackage :io
    (:use :common-lisp)
    (:export
        #:ask
        #:output
        ))

(in-package :io)

(defun ask (question)
    (format *query-io* "~a: " question)
    (force-output *query-io*)
    (read-line *query-io*))

(defun output (object)
    :documentation "prints strings, list, plist"
    (print object)
    (terpri t))
