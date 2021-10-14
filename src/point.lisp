(load "src/random.lisp")

(defun point (&key x y) (list x y))
(defun rect (&key x1 y1 x2 y2) (list (point :x x1 :y y1) (point :x x2 :y y2)))
(defun x? (p) (first p))
(defun y? (p) (second p))
(defun x1? (r) (x (first r)))
(defun y1? (r) (y (first r)))
(defun x2? (r) (x (second r)))
(defun y2? (r) (y (second r)))

(defun make-point-between (&key width height)
  (list
    (random-number-to width)
    (random-number-to height)))

(defun hypotenuse-length (&key point1 point2)
  "Get the distance betweeen two points"
  (let* (
    (x1 (x? point1))
    (x2 (x? point2))
    (y1 (y? point1))
    (y2 (y? point2))
    (a (abs (- x1 x2)))
    (b (abs (- y1 y2)))
    (hypotenuse (sqrt (+ (* a a) (* b b))))
  )
  hypotenuse))

(defun calc-next-point-areas
  (&key
    points ;; List of (x y) ;; At least two points!
    artwork-width
    artwork-height)
  "Get two areas. the first where a point should be,
  second where this point should not be.
  Returns (((x y) (x y)) ((x y) (x y)))"
  (let* (
    (last-two-points (last points 2))
    (p1 (first last-two-points))
    (p2 (second last-two-points))
    (x1 (x? p1))
    (x2 (x? p2))
    (y1 (y? p1))
    (y2 (y? p2))
    (top-left-x (if (> x2 x1) x1 0))
    (bottom-right-x (if (> x2 x1) artwork-width x1))
    (top-left-y (if (> y2 y1) y1 0))
    (bottom-right-y (if (> y2 y1) artwork-height y1))
    (top-left-not-x (if (> x2 x1) x1 x2))
    (bottom-right-not-x (if (> x2 x1) x2 x1))
    (top-left-not-y (if (> y2 y1) y1 y2))
    (bottom-right-not-y (if (> y2 y1) y2 y1)))
    (list
      (rect
        :x1 top-left-x
        :y1 top-left-y
        :x2 bottom-right-x
        :y2 bottom-right-y)
      (rect
        :x1 top-left-not-x
        :y1 top-left-not-y
        :x2 bottom-right-not-x
        :y2 bottom-right-not-y))))

(defun point-in-rect-p (rect point)

  )
