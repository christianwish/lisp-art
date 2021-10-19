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
     :actual (show (list (x&y 1 2) (x&y 3 4) (x&y 5 6) (x&y 7 8)))
     :expected "((X=1.0,Y=2.0),(X=3.0,Y=4.0),(X=5.0,Y=6.0),(X=7.0,Y=8.0))")))

(define-test points-triangle-info-spec
  :func points-triangle-info
  :describe "gives the distance between two points"
  :tests (
    (:test "b, Pythagoreisches Tripel (68 285 293)"
     :actual (b (points-triangle-info (x&y 0 0) (x&y 68 285)))
     :expected 68.0)

    (:test "a, Pythagoreisches Tripel (68 285 293)"
     :actual (a (points-triangle-info (x&y 0 0) (x&y 68 285)))
     :expected 285.0)

    (:test "c (hypotenuse), Pythagoreisches Tripel (68 285 293)"
     :actual (c (points-triangle-info (x&y 0 0) (x&y 68 285)))
     :expected 293.0)

    (:test "alpha, Pythagoreisches Tripel (68 285 293)"
     :actual (alpha (points-triangle-info (x&y 0 0) (x&y 68 285)))
     :expected 76.58032686099308d0)

    (:test "beta, Pythagoreisches Tripel (68 285 293)"
     :actual (beta (points-triangle-info (x&y 0 0) (x&y 68 285)))
     :expected (- 90 76.58032686099308d0))

    (:test "direction 2, Pythagoreisches Tripel (68 285 293)"
     :actual (direction (points-triangle-info (x&y 0 0) (x&y 68 285)))
     :expected 2)

    (:test "direction 1"
     :actual (direction (points-triangle-info (x&y 0 10) (x&y 10 0)))
     :expected 1)

    (:test "direction 3"
     :actual (direction (points-triangle-info (x&y 10 0) (x&y 0 10)))
     :expected 3)

    (:test "direction 4"
     :actual (direction (points-triangle-info (x&y 10 10) (x&y 0 0)))
     :expected 4)))
