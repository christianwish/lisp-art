(defpackage :app
  (:use :common-lisp)
  (:export #:main))

(in-package :app)

(require "io" "./src/io.lisp")
(require "str" "./src/str.lisp")
(load "./src/point.lisp")
(load "./src/svg.lisp")

(defun get-artwork-parameter ()
  (list
    :width (io:ask-with-default "* width in px" "1080")
    :height (io:ask-with-default "* height in px" "1080")
    :padding (io:ask-with-default "* padding in px" "20")
    :max-elements (io:ask-with-default "* max elements" "123")))

(defun main (args)
;;   (io:output (str:format-cyan "First I need some infos
;; for creating the artwork:"))
   (io:output (str:format-yellow "-------------------------"))
;;   (io:output (get-artwork-parameter))
  ;;(print (remove-zero-points (create-path :artwork-width 1080 :artwork-height 1080)))
  (dotimes (i 27) (format t (to-path (remove-zero-points (create-path :artwork-width 1080 :artwork-height 1080))))))
