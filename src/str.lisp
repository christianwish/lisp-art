(defpackage :str
  (:use :common-lisp)
  (:export
    #:concat-all
    #:format-red
    #:format-green
    #:format-cyan
    #:format-yellow
    #:format-purple
    ))

(in-package :str)

(defun concat-all (&rest xs)
  (if (= (list-length xs) 0)
    ""
    (concatenate 'string (first xs) (apply #'concat-all (rest xs)))))


(defmacro make-format-color (&rest colors)
  "generates helper functions for printing colorful output"
  (let ((color-fn #'(lambda (color)
    (let
      ((fn-name (read-from-string (concat-all "format-" (first color))))
        (color-code (second color)))
      `(defun ,fn-name (txt)
        (format nil (concat-all "~c[" ,color-code "m" txt "~c[0m") #\ESC #\ESC))))))
    `(progn ,@(mapcar color-fn colors))))

(make-format-color
  ("cyan" "96")
  ("green" "32")
  ("purple" "35")
  ("yellow" "33")
  ("red" "31"))
