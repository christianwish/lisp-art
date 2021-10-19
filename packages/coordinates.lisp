(defpackage :coordinate
  (:use :common-lisp)
  (:export
    #:coordinate
    #:area
    #:make-coordinate
    #:make-area
    #:show
    ))

(defclass coordinate ()
  ((x
    :initarg :x
    :accessor x
    :type float
    :documentation "X is the distance from left")
   (y
    :initarg :y
    :accessor y
    :type float
    :documentation "Y is the distance from top")))

(defclass area () (
  (c1
    :accessor c1
    :type coordinate)
  (c2
    :accessor c2
    :type coordinate)))

(defun make-coordinate (&key x y)
  "Creates a coordinate with given X and Y value"
  (make-instance 'coordinate :x (float x) :y (float y)))

(defun x&y (x y)
  "Short for `(make-coordinate :x x :y y)`"
  (make-coordinate :x x :y y))

(defun make-area (coor1 coor2)
  "Creates an area of two given coordinates.
  The order of this coordinates is not important and
  will be handeled by this `make-area` function."
  (let* ((new-area (make-instance 'area)))
        (setf (slot-value new-area 'c1) coor1)
        (setf (slot-value new-area 'c2) coor2)
    new-area))

(defun xy&xy (x1 y1 x2 y2)
  "Creates an area of four given coordinates"
  (make-area (make-coordinate :x x1 :y y1) (make-coordinate :x x2 :y y2)))

(defmethod print-object ((obj coordinate) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((x x) (y y)) obj
      (format stream "(X=~a Y=~a)" x y))))

(defmethod print-object ((obj area) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((c1 c1) (c2 c2)) obj
      (format stream "(~a ~a)" c1 c2))))

(defgeneric show (obj))

(defmethod show ((obj coordinate))
  (let ((x (write-to-string (x obj)))
        (y (write-to-string (y obj))))
  (concatenate 'string "(X=" x ",Y=" y ")" )))

(defmethod show ((obj area))
  (concatenate 'string "(" (show (c1 obj)) "," (show (c2 obj)) ")" ))

(defmethod show ((obj null)) "")
(defmethod show ((obj cons))
  (concatenate 'string (show (car obj)) (show (cdr obj))))

(defun coordinate-distance (point1 point2)
  "Get the distance betweeen two points"
  (let* ((x1 (x point1))
         (x2 (x point2))
         (y1 (y point1))
         (y2 (y point2))
         (a (abs (- x1 x2)))
         (b (abs (- y1 y2)))
         (hypotenuse (sqrt (+ (* a a) (* b b)))))
    hypotenuse))
