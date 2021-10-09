(defpackage :app
    (:use :common-lisp)
    (:export
        #:main
        ))

(in-package :app)

(require "io" "./utils/io.lisp")
(require "random" "./utils/random.lisp")

(defun main (args)
    (progn
        (io:output(io:ask "He?"))
        (io:output (random:number-to 39))
    ))
