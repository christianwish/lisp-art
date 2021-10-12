(defpackage :shape
  (:use :common-lisp)
  (:export #:specs #:shape-spec))

(load "test/tdd.lisp")

(deftest shape-spec
  :describe "creates shape data"
  :tests (
    (:test "tests working" :actual 2 :expected 2)
  ))
