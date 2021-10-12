(defpackage :app
  (:use :common-lisp)
  (:export #:main))

(in-package :app)

(require "io" "./src/io.lisp")
(require "random" "./src/random.lisp")
(require "str" "./src/str.lisp")

(defun get-artwork-parameter ()
  (list
    :width (io:ask-with-default "* width in px" "1080")
    :height (io:ask-with-default "* height in px" "1080")
    :max-elements (io:ask-with-default "* max elements" "123")))

(defun main (args)
  (io:output (str:format-cyan "First I need some infos
for creating the artwork:"))
  (io:output (str:format-yellow "-------------------------"))
  (io:output (get-artwork-parameter))
  (io:output (random:number-to 100))
  (io:output (random:number-to 100))
  (io:output (random:number-to 100)))
