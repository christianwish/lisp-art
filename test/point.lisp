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

(deftest hypotenuse-length-spec
  :func hypotenuse-length
  :describe "get distance between two points
Test with 'Pythagoreisches Tripel'"
  :tests (
    (
      :test "Pythagoreisches Tripel (3 4 5) works"
      :actual (let* (
        (p1 '(0 0))
        (p2 '(4 3))
        (c (hypotenuse-length :point1 p1 :point2 p2)))
        c)
      :expected 5.0)
    (
      :test "Pythagoreisches Tripel (15 8 17) works"
      :actual (let* (
        (p1 '(0 0))
        (p2 '(15 8))
        (c (hypotenuse-length :point1 p1 :point2 p2)))
        c)
      :expected 17.0)
    (
      :test "Pythagoreisches Tripel (63 16 65) works"
      :actual (let* (
        (p1 '(0 0))
        (p2 '(63 16))
        (c (hypotenuse-length :point1 p1 :point2 p2)))
        c)
      :expected 65.0)
    (
      :test "Pythagoreisches Tripel (68 285 293) works"
      :actual (let* (
        (p1 '(0 0))
        (p2 '(68 285))
        (c (hypotenuse-length :point1 p1 :point2 p2)))
        c)
      :expected 293.0)
  ))

(deftest calc-next-point-areas-spec
  :func calc-next-point-areas
  :describe "gives coordinates for rec that the
next point should be and
rec in that the point should not be.
Needs at least two points"
  :tests (
    (
      :test "(4 2) (2 4) in (10 10)"
      :actual (calc-next-point-areas
        :points '((4 2) (2 4))
        :artwork-width 10
        :artwork-height 10)
      :expected '(((0 2) (4 10)) ((2 2) (4 4))))
    (
      :test "(2 5) (4 3) in (10 10)"
      :actual (calc-next-point-areas
        :points '((2 5) (4 3))
        :artwork-width 10
        :artwork-height 10)
      :expected '(((2 0) (10 5)) ((2 3) (4 5))))
  ))

(deftest point-in-rect-p-spec
  :func point-in-rect-p
  :describe "Is a given point in a given rect?"
  :tests (
    (
      :test "(5 5) in (0 0) (10 10)"
      :is-true (point-in-rect-p
                  :point (point :x 5 :y 5)
                  :rect (rect :x1 0 :y1 0 :x2 10 :y2 10)))
    (
      :test "(15 5) in (0 0) (10 10)"
      :is-true (not (point-in-rect-p
                  :point (point :x 15 :y 5)
                  :rect (rect :x1 0 :y1 0 :x2 10 :y2 10))))
    (
      :test "(-5 5) in (0 0) (10 10)"
      :is-true (not (point-in-rect-p
                  :point (point :x -5 :y 5)
                  :rect (rect :x1 0 :y1 0 :x2 10 :y2 10))))
    (
      :test "(5 15) in (0 0) (10 10)"
      :is-true (not (point-in-rect-p
                  :point (point :x 5 :y 15)
                  :rect (rect :x1 0 :y1 0 :x2 10 :y2 10))))
  ))

(defun point-specs ()
  (make-point-between-spec)
  (hypotenuse-length-spec)
  (calc-next-point-areas-spec)
  (point-in-rect-p-spec))
