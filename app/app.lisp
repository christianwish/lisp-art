(defpackage :app
    (:use :common-lisp)
    (:export
        #:main
        ))

(in-package :app)

(require "fp" "./lib/utils/fp.lisp")
(require "io" "./lib/utils/io.lisp")
(require "random" "./lib/utils/random.lisp")

(defun main (args) 
    (progn 
        (print args)
    ))