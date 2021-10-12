(defpackage :io
    (:use :common-lisp)
    (:export
        #:ask
        #:ask-with-default
        #:output
        ))

(in-package :io)

(require "str" "./utils/str.lisp")

(defun ask (question)
    (format *query-io* "~a: " question)
    (force-output *query-io*)
    (read-line *query-io*))

(defun ask-with-default (question default-value)
    (let* (
        (answer (ask
            (str:concat-all question " [" default-value "]")))
        (result
            (if (equal answer "") default-value answer))
    ) result))

(defgeneric output (object))

(defmethod output ((object string))
    (format t object)
    (terpri t))

(defmethod output (object)
    (print object)
    (terpri t))
