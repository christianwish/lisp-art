(defpackage :app
  (:use :common-lisp)
  (:export #:main))

(in-package :app)

(require "io" "./utils/io.lisp")
(require "random" "./utils/random.lisp")
(require "str" "./utils/str.lisp")

(defun input-vars ()
  (list
    :width (io:ask-with-default "* width in px" "1080")
    :height (io:ask-with-default "* height in px" "1080")
    :max-elements (io:ask-with-default "* max elements" "123")))

(defun main (args)
  (io:output (str:cyan "First I need some infos
for creating the artwork:
_________________________"))
  (io:output (input-vars)))
