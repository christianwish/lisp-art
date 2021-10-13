(load "test/tdd.lisp")
(load "src/point.lisp")

(deftest make-point-between-spec
  :func make-point-between
  :describe "returns two random numbers in list
`(make-point-between :width NUMBER :height NUMBER)`
-> (Number, Number)"
  :tests (
    (
      :test "make-point-between returns a list with two random numbers"
      :is-true (let* (
        (result (make-point-between :width 100 :height 100))
        (x (first result))
        (y (first result)))
        (and (<= x 100) (>= x 0) (<= y 100) (>= y 0))))

    (
      :test "make-point-between numbers are random"
      :is-true (let* (
        (result1 (make-point-between :width 1000 :height 1000))
        (result2 (make-point-between :width 1000 :height 1000))
        (x (= (first result1) (first result2)))
        (y (= (first result1) (first result2))))
        (not (and x y))))
  ))

(defun point-specs ()
  (make-point-between-spec))
