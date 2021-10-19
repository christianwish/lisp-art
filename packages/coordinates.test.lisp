(load "packages/coordinates.lisp")
(load "test/tdd.lisp")

(define-test-file coordinates)

(define-test show-spec
  :func show
  :describe "gives the string representation of objects"
  :tests (
    (
      :test "gives string for coordinate"
      :actual (show (x&y 3 5))
      :expected "(X=3.0,Y=5.0)")
    (
      :test "gives string for list of coordinate"
      :actual (show (list (x&y 1 2) (x&y 3 4) (x&y 5 6) (x&y 7 8)))
      :expected "((X=1.0,Y=2.0),(X=3.0,Y=4.0),(X=5.0,Y=6.0),(X=7.0,Y=8.0))")
    (
      :test "gives string for area"
      :actual (show (xy&xy 3 5 7 11))
      :expected "((X=3.0,Y=5.0),(X=7.0,Y=11.0))")))

(define-test coordinate-distance-spec
  :func coordinate-distance
  :describe "gives the distance between two points"
  :tests (
    (
      :test "Pythagoreisches Tripel (3 4 5) works"
      :actual (coordinate-distance (x&y 0 0) (x&y 3 4))
      :expected 5.0)
    (
      :test "Pythagoreisches Tripel (15 8 17) works"
      :actual (coordinate-distance (x&y 0 0) (x&y 15 8))
      :expected 17.0)
    (
      :test "Pythagoreisches Tripel (63 16 65) works"
      :actual (coordinate-distance (x&y 0 0) (x&y 63 16))
      :expected 65.0)
    (
      :test "Pythagoreisches Tripel (68 285 293) works"
      :actual (coordinate-distance (x&y 0 0) (x&y 68 285))
      :expected 293.0)
    (
      :test "Pythagoreisches Tripel (68 285 293) translated x and y works"
      :actual (coordinate-distance (x&y 12 15) (x&y 80 300))
      :expected 293.0)))
