(defpackage :shape (:use :common-lisp) (:export #:specs))

(in-package :shape)

(require "tdd" "./utils/tdd.lisp")

(defun specs ()
    (progn
        (tdd:test "shape"
            (tdd:it "tests working" (= 2 2))
            (tdd:it "still working" (= 5 5)))))
