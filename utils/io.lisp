(defpackage :io
    (:use :common-lisp)
    (:export
        #:ask
        #:output
        #:ask-with-default
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

(defun output (object)
    :documentation "prints strings, list, plist"
    (print object)
    (terpri t))
