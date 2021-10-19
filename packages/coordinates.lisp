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

(defmethod print-object ((obj point) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((x x) (y y)) obj
      (format stream "(X=~a Y=~a)" x y))))

(defun points-triangle-info (point1 point2)
  "Two points define a triangle"
  (let* ((x1 (x point1))
         (x2 (x point2))
         (y1 (y point1))
         (y2 (y point2))
         (a (float (abs (- y1 y2))))
         (b (float (abs (- x1 x2))))
         (hypotenuse (float (sqrt (+ (* a a) (* b b)))))
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

(defmethod show ((obj null)) "")
(defmethod show ((obj cons))
  (concatenate 'string (show (car obj)) (show (cdr obj))))
