(defpackage :app
    (:use :common-lisp)
    (:export
        #:main
        ))

(in-package :app)

(require "fp" "./utils/fp.lisp")
(require "io" "./utils/io.lisp")
(require "random" "./utils/random.lisp")

(defun main (args) 
    (progn 
        (print args)
    ))