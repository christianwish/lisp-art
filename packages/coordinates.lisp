(defpackage :coordinates
  (:use :common-lisp)
  (:export
    #:point
    #:area
    #:make-point
    #:make-area
    #:show
    ))

(defclass point ()
  ((x :initarg :x :accessor x :type float
    :documentation "X is the distance from left")
   (y :initarg :y :accessor y :type float
    :documentation "Y is the distance from top")))

(defclass area () (
  (c1 :accessor c1 :type point)
  (c2 :accessor c2 :type point)))

(defclass triangle-info () (
  (a :accessor a :initarg :a :type float)
  (b :accessor b :initarg :b :type float)
  (c :accessor c :initarg :c :type float :documentation "the hypotenuse")
  (alpha :accessor alpha :initarg :alpha :type float)
  (beta :accessor beta :initarg :beta :type float)
  (direction :accessor direction :initarg :direction :type number
   :documentation "like definition of the quadrant (or 1 2 3 4)")))

(defun make-point (&key x y)
  "Creates a point with given X and Y value"
  (make-instance 'point :x (float x) :y (float y)))

(defun x&y (x y)
  "Short for `(make-point :x x :y y)`"
  (make-point :x x :y y))

(defun make-area (coor1 coor2)
  "Creates an area of two given points.
  The order of this points is not important and
  will be handeled by this `make-area` function."
  (let* ((new-area (make-instance 'area)))
        (setf (slot-value new-area 'c1) coor1)
        (setf (slot-value new-area 'c2) coor2)
    new-area))

(defun xy&xy (x1 y1 x2 y2)
  "Creates an area of four given points"
  (make-area (make-point :x x1 :y y1) (make-point :x x2 :y y2)))

(defmethod print-object ((obj point) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((x x) (y y)) obj
      (format stream "(X=~a Y=~a)" x y))))

(defmethod print-object ((obj area) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((c1 c1) (c2 c2)) obj
      (format stream "(~a ~a)" c1 c2))))

(defun points-triangle-info (point1 point2)
  "Two points define a triangle"
  (let* ((x1 (x point1))
         (x2 (x point2))
         (y1 (y point1))
         (y2 (y point2))
         (a (float (abs (- y1 y2))))
         (b (float (abs (- x1 x2))))
         (hypotenuse (sqrt (+ (* a a) (* b b))))
         (alpha (* (atan (/ a b)) (/ 180 pi)))
         (direction (cond
                      ((and (<= x1 x2) (>= y1 y2)) 1)
                      ((and (<= x1 x2) (<= y1 y2)) 2)
                      ((and (>= x1 x2) (<= y1 y2)) 3)
                      (t 4))))
        (make-instance 'triangle-info
          :a a
          :b b
          :c (float hypotenuse)
          :alpha (float alpha)
          :beta (- 90 alpha)
          :direction direction)))

;;;; show -------------------------------------------------
(defgeneric show (obj))

(defmethod show ((obj point))
  (let ((x (write-to-string (x obj)))
        (y (write-to-string (y obj))))
  (concatenate 'string "(X=" x ",Y=" y ")" )))

(defmethod show ((obj area))
  (concatenate 'string "(" (show (c1 obj)) "," (show (c2 obj)) ")" ))

(defmethod show ((obj null)) "")
(defmethod show ((obj cons))
  (concatenate 'string (show (car obj)) (show (cdr obj))))
