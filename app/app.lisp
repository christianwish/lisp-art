(defpackage :app
  (:use :common-lisp)
  (:export #:main))

(in-package :app)

(load "./src/io.lisp")
(load "./src/str.lisp")
(load "./src/point.lisp")
(load "./src/svg.lisp")

(defun get-artwork-parameter ()
  (list
    :width (ask-with-default "* width in px" "1080")
    :height (ask-with-default "* height in px" "1080")
    :padding (ask-with-default "* padding in px" "20")
    :max-elements (ask-with-default "* max elements" "123")))

(defun main (args)
  (let* ((result (list ""))
         (paths-list (dotimes (i 23) (push (svg/path :width 1080 :height 1080) result)))
         (paths (apply #'concatenate 'string result)))
  (create-svg-file paths :width 1080 :height 1080)))
