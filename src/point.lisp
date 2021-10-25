(load "src/random.lisp")

(defun point (&key x y) (list x y))
(defun rect (&key x1 y1 x2 y2)
  (list (point :x x1 :y y1) (point :x x2 :y y2)))
(defun point/x (p) (first p))
(defun point/y (p) (second p))
(defun rect/x1 (r) (point/x (first r)))
(defun rect/y1 (r) (point/y (first r)))
(defun rect/x2 (r) (point/x (second r)))
(defun rect/y2 (r) (point/y (second r)))

(defun make-point-between (&key width height)
  (list
    (random-number-to width)
    (random-number-to height)))

(defun hypotenuse-length (&key point1 point2)
  "Get the distance betweeen two points"
  (let* ((x1 (point/x point1))
         (x2 (point/x point2))
         (y1 (point/y point1))
         (y2 (point/y point2))
         (a (abs (- x1 x2)))
         (b (abs (- y1 y2)))
         (hypotenuse (sqrt (+ (* a a) (* b b)))))
    hypotenuse))

(defun calc-next-point-areas
  (&key
    points ;; List of (x y) ;; At least two points!
    width
    height)
  "Get two areas. the first where a point should be,
  second where this point should not be.
  Returns (((x y) (x y)) ((x y) (x y)))"
  (let* ((last-two-points (last points 2))
         (p1 (first last-two-points))
         (p2 (second last-two-points))
         (x1 (point/x p1))
         (x2 (point/x p2))
         (y1 (point/y p1))
         (y2 (point/y p2))
         (top-left-x (if (> x2 x1) x1 0))
         (bottom-right-x (if (> x2 x1) width x1))
         (top-left-y (if (> y2 y1) y1 0))
         (bottom-right-y (if (> y2 y1) height y1))
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

(defun point-in-rect-p (&key rect point)
  (let* ((x1 (rect/x1 rect))
         (x2 (rect/x2 rect))
         (y1 (rect/y1 rect))
         (y2 (rect/y2 rect))
         (x (point/x point))
         (y (point/y point)))
    (and (>= x x1) (<= x x2) (>= y y1) (<= y y2))))

(defun random-point-in-rect (r)
  (point
    :x (random-number-from-to (rect/x1 r) (rect/x2 r))
    :y (random-number-from-to (rect/y1 r) (rect/y2 r))))

(defun create-next-point (&key points width height)
  (let* ((areas (calc-next-point-areas
                  :points points
                  :width width
                  :height height))
         (yes-react (first areas))
         (no-area (second areas))
         (last-two-points (last points 2))
         (p1 (first last-two-points))
         (p2 (second last-two-points))
         (points-distance (hypotenuse-length :point1 p1 :point2 p2)))
    (defun run! (&optional (rounds 0))
      (if (>= rounds 1000)
          (random-point-in-rect yes-react)
          (let* ((random-point (random-point-in-rect yes-react))
                 (distance-to-p1 (hypotenuse-length :point1 p1 :point2 random-point))
                 (valid-point-p
                    (and (> distance-to-p1 points-distance)
                         (not (point-in-rect-p :rect no-area :point random-point)))))
          (if valid-point-p random-point (run! (+ rounds 1))))))
    (run!)))

(defun create-path-points (&key width height)
  (let* ((start-points
            (list
              (random-point-in-rect (rect :x1 0 :y1 0 :x2 width :y2 height))
              (random-point-in-rect (rect :x1 0 :y1 0 :x2 width :y2 height))))
         (point-length (random-number-from-to 3 7)))

    (defun run-it (points)
      (if (= (length points) point-length)
          points
          (run-it (append points (list (create-next-point
                                        :points points
                                        :width width
                                        :height height))))))
    (run-it start-points)))

(defun remove-zero-points (points)
  (remove-if (function (lambda (point) (or (= (point/x point) 0) (= (point/y point) 0)))) points))
