(load "packages/coordinates.lisp")
(load "test/tdd.lisp")

(define-test-file points)

(define-test show-spec
  :func show
  :describe "gives the string representation of objects"
  :tests (
    (:test "gives string for point"
     :actual (show (x&y 3 5))
     :expected "(X=3.0,Y=5.0)")

    (:test "gives string for list of points"
     :skip 'yes
     :actual (show (list (x&y 1 2) (x&y 3 4) (x&y 5 6) (x&y 7 8)))
     :expected "((X=1.0,Y=2.0),(X=3.0,Y=4.0),(X=5.0,Y=6.0),(X=7.0,Y=8.0))")))

(define-test make-triangle-info-spec
  :func make-triangle-info
  :tests (
    (:test "b, Pythagoreisches Tripel (68 285 293)"
     :actual (b (make-triangle-info (x&y 0 0) (x&y 68 285)))
     :expected 68.0)

    (:test "a, Pythagoreisches Tripel (68 285 293)"
     :actual (a (make-triangle-info (x&y 0 0) (x&y 68 285)))
     :expected 285.0)

    (:test "c (hypotenuse), Pythagoreisches Tripel (68 285 293)"
     :actual (c (make-triangle-info (x&y 0 0) (x&y 68 285)))
     :expected 293.0)

    (:test "alpha, Pythagoreisches Tripel (68 285 293)"
     :actual (alpha (make-triangle-info (x&y 0 0) (x&y 68 285)))
     :expected 76.58032686099308d0)

    (:test "alpha of a horizontal line, left to right, is rounded zero"
     :actual (round (alpha (make-triangle-info (x&y 0 0) (x&y 10 0))))
     :expected 0)

    (:test "alpha of a horizontal line, right to left, is rounded zero"
     :actual (round (alpha (make-triangle-info (x&y 10 0) (x&y 0 0))))
     :expected 0)

    (:test "alpha of a vertical line, bottom to top, is rounded zero"
     :actual (round (alpha (make-triangle-info (x&y 0 10) (x&y 0 0))))
     :expected 0)

    (:test "alpha of a vertical line, top to bottom, is rounded zero"
     :actual (round (alpha (make-triangle-info (x&y 0 0) (x&y 0 10))))
     :expected 0)

    (:test "alpha of a two indeticals points is rounded 0"
     :actual (round (alpha (make-triangle-info (x&y 10 10) (x&y 10 10))))
     :expected 0)

    (:test "beta, Pythagoreisches Tripel (68 285 293)"
     :actual (beta (make-triangle-info (x&y 0 0) (x&y 68 285)))
     :expected (- 90 76.58032686099308d0))

    (:test "alpha-absolute in direction 1"
     :actual (round (alpha-absolute (make-triangle-info (x&y 0 10) (x&y 10 0))))
     :expected 45)

    (:test "alpha-absolute in direction 2"
     :actual (round (alpha-absolute (make-triangle-info (x&y 0 0) (x&y 10 10))))
     :expected 135)

    (:test "alpha-absolute in direction 3"
     :actual (round (alpha-absolute (make-triangle-info (x&y 10 0) (x&y 0 10))))
     :expected 225)

    (:test "alpha-absolute in direction 4"
     :actual (round (alpha-absolute (make-triangle-info (x&y 10 10) (x&y 0 0))))
     :expected 315)

    (:test "alpha-absolute of a horizontal line, left to right"
     :actual (round (alpha-absolute (make-triangle-info (x&y 0 0) (x&y 10 0))))
     :expected 90)

    (:test "alpha-absolute of a horizontal line, right to left"
     :actual (round (alpha-absolute (make-triangle-info (x&y 10 0) (x&y 0 0))))
     :expected 270)

    (:test "alpha-absolute of a vertical line, bottom to top"
     :actual (round (alpha-absolute (make-triangle-info (x&y 0 10) (x&y 0 0))))
     :expected 0)

    (:test "alpha-absolute of a vertical line, top to bottom"
     :actual (round (alpha-absolute (make-triangle-info (x&y 0 0) (x&y 0 10))))
     :expected 180)

    (:test "alpha-absolute of a two indeticals points is rounded 0"
     :actual (round (alpha-absolute (make-triangle-info (x&y 10 10) (x&y 10 10))))
     :expected 0)))
