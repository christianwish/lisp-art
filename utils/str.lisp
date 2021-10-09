(defpackage :str
    (:use :common-lisp)
    (:export
        #:concat-all
        #:red
        #:blue
        #:green
        #:cyan
        #:yellow
        #:purple
        ))

(in-package :str)

(defun concat-all (&rest xs)
    (if (= (list-length xs) 0)
        ""
        (concatenate 'string (first xs)
            (apply #'concat-all (rest xs) ))))

(defun red (str)
    (format t
     (concat-all "~c[31m" str "~c[0m~%") #\ESC #\ESC))

(defun blue (str)
    (format t
     (concat-all "~c[34m" str "~c[0m~%") #\ESC #\ESC))

(defun green (str)
    (format t
     (concat-all "~c[32m" str "~c[0m~%") #\ESC #\ESC))

(defun cyan (str)
    (format t
     (concat-all "~c[36m" str "~c[0m~%") #\ESC #\ESC))

(defun yellow (str)
    (format t
     (concat-all "~c[33m" str "~c[0m~%") #\ESC #\ESC))

(defun purple (str)
    (format t
     (concat-all "~c[35m" str "~c[0m~%") #\ESC #\ESC))
