(defpackage :app
  (:use :common-lisp)
  (:export #:main))

(in-package :app)

(load "./src/io.lisp")
(load "./src/str.lisp")
(load "./src/point.lisp")
(load "./src/svg.lisp")

(defun main (args)
  (let* ((width (parse-integer (ask-with-default "* width in px" "1080")))
         (height (parse-integer (ask-with-default "* height in px" "1080")))
         (max-elements (parse-integer (ask-with-default "* max elements" "7")))
         (result (list ""))
         (paths-list (dotimes (i max-elements) (push (svg/path :width width :height height) result)))
         (paths (apply #'concatenate 'string result))
         )
  (create-svg-file paths :width width :height height)))
